#' Calculating unadjusted and adjusted differences in rates
#'
#' This function calculates the unadjusted or adjusted difference in rates with
#' confidence interval.
#'
#' @param data A data frame
#' @param y vector of binary outcome variables. Outcome variables can be
#' numeric, character or factor, but must have two and only two non-missing levels
#' @param x string indicating the binary stratifying variable. The stratifying
#' variable can be numeric, character or factor, but must have two and only two
#' non-missing levels
#' @param formula By default, "{y} ~ {x}". To include covariates for an adjusted
#' risk difference, add covariate names to the formula, e.g. "{y} ~ {x} + age"
#' @param method The method for calculating confidence intervals around the
#' difference in rates. The options are "exact", for an unadjusted difference
#' in rates, or "boot_centile" or "boot_sd" for an adjusted difference in rates.
#' The default method is "exact".
#' @param conf.level Confidence level of the returned confidence interval.
#' Must be a single number between 0 and 1. The default is a 95% confidence interval.
#' @param bootstrapn The number of bootstrap resamples to use. The default is 2000
#' for "boot_centile" and 200 for "boot_sd"
#' @param estimate_fun Function to round and format estimates. By default
#' `style_sigfig`, but can take any formatting function
#' @param pvalue_fun Function to round and format p-value. By default
#' `style_pvalue`, but can take any formatting function
#'
#' @return A `tbl_propdiff` object
#' @export
#'
#' @section Methods:
#'
#' The `exact`` option gives the exact confidence interval for the unadjusted
#' difference in proportions as calculated by `prop.test`.
#'
#' The `boot_centile` option calculates the adjusted difference between groups
#' in all bootstrap samples (the default for this method is 2000 resamples)
#' and generates the confidence intervals from the distribution of these
#' differences. For the default, a 95% confidence interval, the 2.5 and 97.5
#' centiles are used.
#'
#' The `boot_sd` option calculates the adjusted difference between groups
#' in all bootstrap samples (the default for this method is 200 resamples).
#' The mean and standard deviation of the adjusted difference across all
#' resamples are calculated. The standard deviation is then used as the
#' standard error to calculate the confidence interval based on the true adjusted difference.
#'
#' @examples
#' tbl_propdiff(
#'   data = trial,
#'   y = "response",
#'   x = "trt"
#' )
#'
#' tbl_propdiff(
#'   data = trial,
#'   y = "response",
#'   x = "trt",
#'   formula = "{y} ~ {x} + age + stage",
#'   method = "boot_sd",
#'   bootstrapn = 250
#' )
#' #
tbl_propdiff <- function(data, y, x, formula = "{y} ~ {x}",
                         method = c("exact", "boot_sd", "boot_centile"),
                         conf.level = 0.95,
                         bootstrapn = ifelse(method == "boot_centile", 2000, 200),
                         estimate_fun = style_sigfig,
                         pvalue_fun = style_pvalue) {

  ### CHECKS------------------

  # browser()

  # Matching arguments for method
  method <- match.arg(method)

  # Save out list of covariates from formula
  rhs <- map(stringr::str_split(rlang::f_text(as.formula(glue(formula))), pattern = "\\+"), ~ stringr::str_trim(.x)) %>% pluck(1)
  covariates <- setdiff(rhs, x)

  # Confirm that x and y variables and covariates exist
  if (length(setdiff(c(x, y, covariates), names(data))) != 0) {
    stop(glue(
      "These variables do not exist in the dataset: ",
      glue_collapse(setdiff(c(x, y), names(data)), sep = ", ")
    ),
    call. = FALSE
    )
  }

  # converting inputs to strings/lists
  y <- dplyr::select(data[0, , drop = FALSE], {{ y }}) %>% names()
  x <- dplyr::select(data[0, , drop = FALSE], {{ x }}) %>% names()

  # checking the x variable has two levels
  if (data[[x]] %>% stats::na.omit() %>% unique() %>% length() != 2) {
    stop(glue::glue("The stratifying variable, '{x}', must have two levels."),
      call. = FALSE
    )
  }

  # checking the y variables have two levels
  if (purrr::every(y, function(x) {
    data[[x]] %>%
      stats::na.omit() %>%
      unique() %>%
      length() == 2
  }) == FALSE) {
    stop(glue::glue("All outcome variables, '{y}', must have two levels."),
      call. = FALSE
    )
  }

  # Confirm that conf.level is not < 0 or > 1
  if (conf.level < 0 | conf.level > 1) {
    stop("The confidence level specified in the `conf.level=` option must be between 0 and 1.",
      call. = FALSE
    )
  }

  # TODO: Should we print a message that "exact" method used if no other
  # method specified, even if covariates are given in formula?

  # Checking estimate_fun and pvalue_fun are functions
  if (!purrr::every(list(estimate_fun, pvalue_fun), is.function)) {
    stop("Inputs `estimate_fun` and `pvalue_fun` must be functions.",
      call. = FALSE
    )
  }

  ### DATAFRAME FOR ALL MODELS--------------------------------------

  df_propdiff <-
    tibble::enframe(y, name = NULL, value = "y") %>%
    mutate(x = x)

  # Reverse factor levels for x variable to match tbl_ancova output
  # Convert y to factor
  data <-
    data %>%
    mutate_at(vars(x), ~ forcats::fct_rev(factor(.))) %>%
    mutate_at(vars(y), ~ factor(.))

  ### UNADJUSTED RATES---------------------------------

  df_propdiff_summary <-
    df_propdiff %>%
    mutate(
      # Before creating tables, save out outcome label
      outcome_label =
        map_chr(
          y,
          ~ ifelse(!is.null(attr(data[[..1]], "label")),
            attr(data[[..1]], "label"),
            y
          )
        ),
      # Save out table of unadjusted rates
      tbl_rates =
        pmap(
          list(x, y, outcome_label),
          function(x, y, z) {
            data %>%
              select(tidyselect::all_of(x), tidyselect::all_of(y)) %>%
              tbl_summary(
                by = .data[[x]], missing = "no",
                label = list(x = glue("{z}")),
                type = list(all_categorical() ~ "dichotomous")
              ) %>%
              add_n() %>%
              modify_header(stat_by = gt::md("**{level}**"))
          }
        )
    )
  # TODO: Bring up with Dan that tbl_ancova is giving error for "response" when it shouldn't
  # because 3rd level is missing
  # TODO: What about tbl_summary warnings?

  ### CALCULATE DIFFERENCES-------------------------

  # For "exact" method
  if (method == "exact") {
    df_propdiff_final <-
      df_propdiff_summary %>%
      mutate(
        estci =
          pmap(
            list(x, y),
            ~ calculate_exact(
              data = data %>%
                select(tidyselect::all_of(..1), tidyselect::all_of(..2)) %>%
                filter(complete.cases(.) == TRUE),
              x = ..1,
              y = ..2,
              conf.level = conf.level
            )
          )
      )

    # For multivariable bootstrap methods
  } else if (method %in% c("boot_centile", "boot_sd")) {

    # Calculate central estimate in full dataset
    df_propdiff_est <-
      df_propdiff_summary %>%
      mutate(
        est =
          pmap(
            list(x, y),
            ~ create_model_pred(
              data = data %>%
                select(tidyselect::all_of(..1), tidyselect::all_of(..2), tidyselect::all_of(covariates)) %>%
                filter(complete.cases(.) == TRUE),
              x = ..1,
              y = ..2,
              covariates = covariates,
              pvalue = TRUE
            )
          )
      )

    # Create list of indicators for each resample, separately for each outcome
    df_bs_map <-
      df_propdiff_est %>%
      select(.data$x, .data$y) %>%
      mutate(freq = bootstrapn) %>%
      tidyr::uncount(freq) %>%
      mutate(
        nrow =
          pmap_int(
            list(x, y),
            ~ nrow(
              data %>%
                select(tidyselect::all_of(..1), tidyselect::all_of(..2), tidyselect::all_of(covariates))
            )
          ),
        bs_assignment =
          map(
            nrow, ~ sample.int(..1, replace = TRUE)
          ),
        # Bootstrapping adjusted difference
        bs_pred =
          pmap(
            list(x, y, bs_assignment),
            ~ create_model_pred(
              data = data %>%
                select(tidyselect::all_of(..1), tidyselect::all_of(..2), tidyselect::all_of(covariates)) %>%
                filter(complete.cases(.) == TRUE) %>%
                slice(..3),
              x = ..1,
              y = ..2,
              covariates = covariates,
              pvalue = TRUE
            )
          )
      ) %>%
      select(.data$x, .data$y, .data$bs_pred) %>%
      unnest(.data$bs_pred) %>%
      nest(bs_pred = -c(.data$x, .data$y))
  }

  #### CALCULATE 95% CI---------------------------------

  # 95% CI for exact method already calculated

  # Calculate which centiles to use
  lower_centile <- (1 - conf.level) / 2
  upper_centile <- 1 - (1 - conf.level) / 2

  # For centile method
  if (method == "boot_centile") {

    # Calculate 95% CI using centiles
    df_propdiff_ci <-
      df_bs_map %>%
      mutate(
        ci =
          map(
            .data$bs_pred,
            ~ ..1 %>%
              summarize(
                conf.low_2 = quantile(.data$estimate_2, lower_centile, na.rm = TRUE),
                conf.high_2 = quantile(.data$estimate_2, upper_centile, na.rm = TRUE)
              )
          )
      ) %>%
      select(-.data$bs_pred)

    # Merge with main results
    df_propdiff_final <-
      left_join(
        df_propdiff_est,
        df_propdiff_ci,
        by = c("x", "y")
      ) %>%
      mutate(
        estci = map2(est, ci, ~ bind_cols(..1, ..2))
      ) %>%
      select(-est, -ci)

    # For mean/SD method
  } else if (method == "boot_sd") {

    # Calculate standard deviation around mean differences
    df_propdiff_ci <-
      df_bs_map %>%
      mutate(
        se =
          map_dbl(
            .data$bs_pred,
            ~ ..1 %>%
              summarize(se = sd(.data$estimate_2, na.rm = TRUE)) %>%
              pull(se)
          )
      ) %>%
      select(-.data$bs_pred)

    # Merge with main results
    df_propdiff_final <-
      left_join(
        df_propdiff_est,
        df_propdiff_ci,
        by = c("x", "y")
      ) %>%
      mutate(
        estci =
          map2(
            est, se,
            ~ ..1 %>%
              mutate(
                conf.low_2 = .data$estimate_2 + stats::qnorm(lower_centile) * ..2,
                conf.high_2 = .data$estimate_2 + stats::qnorm(upper_centile) * ..2
              )
          )
      ) %>%
      select(-est, -se)
  }

  # Standardize format of results
  df_propdiff_fmt <-
    df_propdiff_final %>%
    mutate(
      estci =
        map(
          estci,
          ~ ..1 %>%
            mutate_at(
              vars(.data$estimate_2, .data$conf.low_2, .data$conf.high_2),
              ~ . * 100
            ) %>%
            mutate(
              ci = as.character(glue("{estimate_fun(.data$conf.low_2)}%, {estimate_fun(.data$conf.high_2)}%"))
            )
        )
    )

  # Stack tbl_summary tables
  tbl_results <-
    tbl_stack(df_propdiff_fmt$tbl_rates)

  # Unnest difference and 95% CI
  df_estci <-
    df_propdiff_fmt %>%
    select(estci) %>%
    unnest(cols = c(estci)) %>%
    select(estimate_2, ci, conf.low_2, conf.high_2, p.value_2)

  # Add results to table body
  tbl_results$table_body <-
    bind_cols(
      tbl_results$table_body,
      df_estci
    ) %>%
    rename(stat_1_1 = stat_1, stat_2_1 = stat_2)

  # Update table header
  if (method == "exact") estlabel <- "**Difference**" else estlabel <- "**Adjusted Difference**"

  # Update header
  tbl_results$table_header <-
    left_join(
      tibble(column = names(tbl_results$table_body)),
      tbl_results$table_header %>%
        mutate(
          column =
            dplyr::case_when(
              column == "stat_1" ~ "stat_1_1",
              column == "stat_2" ~ "stat_2_1",
              TRUE ~ column
            )
        ),
      by = "column"
    ) %>%
    gtsummary:::table_header_fill_missing() %>%
    gtsummary:::table_header_fmt_fun(
      estimate_2 = function(x) as.character(glue("{estimate_fun(x)}%")),
      p.value_2 = pvalue_fun
    )

  tbl_results <-
    gtsummary:::modify_header_internal(
      tbl_results,
      estimate_2 = estlabel,
      ci = "**95% CI**",
      p.value_2 = "**p-value**"
    )

  # Update gt calls
  tbl_results <- gtsummary:::update_calls_from_table_header(tbl_results)

  # Add class
  class(tbl_results) <- c("tbl_propdiff", "gtsummary")

  return(tbl_results)
}
