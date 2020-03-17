#' Calculating unadjusted and adjusted differences in rates
#'
#' This function calculates the unadjusted or adjusted difference in rates with
#' confidence interval.
#'
#' @param data A data frame
#' @param outcome A vector of outcome variable names. Outcome variables can be
#' numeric, character or factor, but must have two and only two non-missing levels
#' @param y vector of binary outcome variables
#' @param x string indicating the binary comparison variable
#' @param formula
#' @param method The method for calculating confidence intervals around the
#' adjusted difference in rates. The 'centile' option is the default and uses
#' 2000 bootstrap resamples by default and calculates the confidence interval
#' using centiles (by default, 2.5th and 97.5th centiles for a 95% confidence
#' interval). The 'mean' option can also be selected, which uses a default 200
#' bootstrap resamples and uses the standard deviation for the mean adjusted
#' difference across all resamples to calculate the confidence interval.
#' @param conf.level Confidence level of the returned confidence interval.
#' Must be a single number between 0 and 1. The default is a 95% confidence interval.
#' @param bootstrapn The number of bootstrap resamples to use. The default is
#' 2000 when `method = "centile"` and the default is 200 when `method = "mean"`
#' @param estimate_fun Function to round and format estimates. By default
#' `style_sigfig`, but can take any formatting function
#' @param pvalue_fun Function to round and format p-value. By default
#' `style_pvalue`, but can take any formatting function
#'
#' @return A formatted `gt` table with the unadjusted rate in each group,
#' and the difference in rates, confidence interval and p-value for the comparison
#' (unadjusted by default, adjusted if `covariates` option specified)
#' @export
#'
#' @examples
#' tbl_propdiff(
#'   data = trial,
#'   outcome = "response",
#'   predictor = "trt"
#' )
#'
#' tbl_propdiff(
#'   data = trial,
#'   outcome = "response",
#'   predictor = "trt",
#'   covariates = c("age", "stage"),
#'   method = "mean",
#'   bootstrapn = 250
#' )
tbl_propdiff <- function(data, y, x, formula = "{y} ~ {x}",
                         method = c("exact", "boot_sd", "boot_centile"),
                         conf.level = 0.95,
                         bootstrapn = ifelse(method == "boot_centile", 2000, 200),
                         estimate_fun = style_sigfig,
                         pvalue_fun = style_pvalue) {

  ### CHECKS------------------

  # browser()

  # Confirm that outcome, predictor and covariates exist
  if (length(setdiff(c(predictor, outcome), names(data))) != 0) {
    stop(glue(
      "These variables do not exist in the dataset: ",
      glue_collapse(setdiff(c(predictor, outcome), names(data)), sep = ", ")
    ),
    call. = FALSE
    )
  }

  # Check that outcomes and predictor are binary
  if (data[!is.na(data[[predictor]]), ][[predictor]] %>% unique() %>% length() != 2) {
    stop(glue("The stratifying variable, '{predictor}', must have two and only two non-missing levels."),
      call. = FALSE
    )
  }
  if (purrr::every(outcome, function(x) {
    data[!is.na(data[[x]]), ][[x]] %>%
      unique() %>%
      length() == 2
  }) == FALSE) {
    stop(glue(
      "All outcome variables (",
      glue_collapse(outcome, sep = ","),
      ") must have two and only two non-missing levels."
    ),
    call. = FALSE
    )
  }

  # Confirm that conf.level is not < 0 or > 1
  if (conf.level < 0 | conf.level > 1) {
    stop("The confidence level specified in the `conf.level=` option must be between 0 and 1.",
      call. = FALSE
    )
  }

  # TODO: Warning if there are covariates provided for method exact?

  # Matching arguments for method
  method <- match.arg(method)}

  # Confirm no variables in dataset that match temporary variable names
  if (any(c("..predname..", "..prednum..", "..outname..", "..outnum..") %in% names(data)) == TRUE) {
    stop(
      glue(
        "The following variable(s) already exist in this dataset: ",
        glue::glue_collapse(
          intersect(
            c("..predname..", "..prednum..", "..outname..", "..outnum.."),
            names(data)
          ),
          sep = ", "
        )
      ),
      call. = FALSE
    )
  }

  # Checking estimate_fun and pvalue_fun are functions
  if (!purrr::every(list(estimate_fun, pvalue_fun), is.function)) {
    stop("Inputs `estimate_fun` and `pvalue_fun` must be functions.",
      call. = FALSE
    )
  }

  ### DATAFRAME FOR ALL MODELS--------------------------------------

  df_propdiff <-
    list(y = y, x = x) %>%
    purrr::cross_df() %>%

  ### SETUP-------------------

  # Convert to strings
  predictor <- select(data[0, ], {{ predictor }}) %>% names()
  outcome <- select(data[0, ], {{ outcome }}) %>% names()

  # Dataset excluding patients missing predictor (excluded from all analyses)
  df_full <-
    data[!is.na(data[[predictor]]), ] %>%
    rename(..predname.. = tidyselect::all_of(predictor)) %>%
    mutate(
      ..prednum.. = dplyr::group_indices(., .data$..predname..) - 1
    )

  # Pull out column labels to use for table (correctly label after flipping)
  column_labels <-
    df_full %>%
    select(.data$..prednum.., .data$..predname..) %>%
    unique() %>%
    arrange(.data$..prednum..) %>%
    pull(.data$..predname..)

  # Pull out outcome labels (editing dataset drops labels)
  outcome_labels <-
    data %>%
    select(tidyselect::all_of(outcome)) %>%
    map(~ attr(.x, "label"))

  ### SUMMARY OF UNADJUSTED RATES-----------------------------------

  # Create table of unadjusted rates
  tbl_results <-
    df_full %>%
    select(.data$..prednum.., tidyselect::all_of(outcome)) %>%
    mutate(..prednum.. = factor(.data$..prednum.., labels = column_labels)) %>%
    tbl_summary(by = .data$..prednum.., missing = "no", label = outcome_labels)

  ### UNADJUSTED ESTIMATES AND DIFFERENCE------------------

  if (is.null(covariates)) {

    # Univariate difference
    df_results <-
      tibble(outcome = outcome) %>%
      mutate(
        # Calculate difference, CI, p-value
        df_uvresults =
          map(
            outcome,
            ~ calculate_uvpropdiff(
              data = df_full[!is.na(df_full[[..1]]), ] %>%
                rename(..outname.. = tidyselect::all_of(..1)) %>%
                mutate(..outnum.. = dplyr::group_indices(., .data$..outname..) - 1) %>%
                select(.data$..prednum.., .data$..outnum.., tidyselect::all_of(covariates)) %>%
                filter(stats::complete.cases(.) == TRUE),
              x = "..prednum..", y = "..outnum..", conf.level = conf.level
            )
          )
      ) %>%
      select(.data$outcome, .data$df_uvresults) %>%
      unnest(.data$df_uvresults)
  }

  ### ADJUSTED ESTIMATES-----------------

  if (!is.null(covariates)) {

    # Calculate adjusted difference for full dataset
    df_results_map <-
      tibble(outcome = outcome) %>%
      mutate(
        df_prediction =
          map(
            outcome,
            ~ create_model_pred(
              data = df_full[!is.na(df_full[[..1]]), ] %>%
                rename(..outname.. = tidyselect::all_of(..1)) %>%
                mutate(..outnum.. = dplyr::group_indices(., .data$..outname..) - 1) %>%
                select(.data$..prednum.., .data$..outnum.., tidyselect::all_of(covariates)) %>%
                filter(stats::complete.cases(.) == TRUE),
              y = "..outnum..",
              x = "..prednum..",
              covariates = covariates,
              pvalue = TRUE
            )
          )
      ) %>%
      unnest(.data$df_prediction) %>%
      select(.data$outcome, .data$estimate, .data$p.value)

    ### BOOTSTRAP MODELS------------------
    # Creation of the models/predictions is the same for both methods
    # Calculate of the CI at the end is different

    # Create list of indicators for each resample, separately for each outcome
    df_bs_map <-
      list(
        outcome = outcome,
        bsn = 1:bootstrapn
      ) %>%
      purrr::cross_df() %>%
      mutate(
        nrow =
          map_int(
            outcome,
            ~ nrow(df_full[!is.na(..1), ] %>%
              select(..prednum.., tidyselect::all_of(covariates)) %>%
              filter(stats::complete.cases(.) == TRUE))
          ),
        bs_assignment =
          map(
            nrow, ~ sample.int(..1, replace = TRUE)
          ),
        # Bootstrapping adjusted difference
        df_prediction =
          map2(
            .data$outcome, .data$bs_assignment,
            ~ create_model_pred(
              data = df_full[!is.na(df_full[[..1]]), ] %>%
                rename(..outname.. = tidyselect::all_of(..1)) %>%
                mutate(..outnum.. = dplyr::group_indices(., .data$..outname..) - 1) %>%
                select(.data$..prednum.., .data$..outnum.., tidyselect::all_of(covariates)) %>%
                filter(stats::complete.cases(.) == TRUE) %>%
                slice(..2),
              x = "..prednum..",
              y = "..outnum..",
              covariates = covariates
            )
          )
      ) %>%
      select(.data$outcome, .data$df_prediction) %>%
      unnest(.data$df_prediction) %>%
      nest(df_prediction = -c(.data$outcome))

    ### CALCULATE CONFIDENCE INTERVALS----------------------------

    # Calculate which centiles to use
    lower_centile <- (1 - conf.level) / 2
    upper_centile <- 1 - (1 - conf.level) / 2

    # For centile method
    if (method == "centile") {

      # Calculate 95% CI using centiles
      df_results <-
        df_bs_map %>%
        mutate(
          df_ci_results =
            map(
              .data$df_prediction,
              ~ ..1 %>%
                summarize(
                  conf.low = quantile(.data$estimate, lower_centile, na.rm = TRUE),
                  conf.high = quantile(.data$estimate, upper_centile, na.rm = TRUE)
                )
            )
        ) %>%
        select(-.data$df_prediction) %>%
        unnest(.data$df_ci_results) %>%
        # Merge in main results
        left_join(
          df_results_map,
          by = "outcome"
        )
    } else if (method == "mean") {

      # Calculate standard deviation around mean differences
      df_results <-
        df_bs_map %>%
        mutate(
          se =
            purrr::map_dbl(
              .data$df_prediction,
              ~ ..1 %>%
                summarize(se = sd(.data$estimate, na.rm = TRUE)) %>%
                pull(se)
            )
        ) %>%
        select(-.data$df_prediction) %>%
        # Merge in main results
        left_join(
          df_results_map,
          by = "outcome"
        ) %>%
        mutate(
          conf.low = .data$estimate + stats::qnorm(lower_centile) * .data$se,
          conf.high = .data$estimate + stats::qnorm(upper_centile) * .data$se
        ) %>%
        select(-.data$se)
    }
  }

  # Standardize format of results
  df_results <-
    df_results %>%
    dplyr::transmute(
      variable = .data$outcome,
      estimate = .data$estimate * 100,
      conf.low = .data$conf.low * 100,
      conf.high = .data$conf.high * 100,
      ci = glue("{estimate_fun(.data$conf.low)}%, {estimate_fun(.data$conf.high)}%"),
      .data$p.value
    )

  # Add results to table body
  tbl_results$table_body <-
    left_join(
      tbl_results$table_body,
      df_results,
      by = "variable"
    )

  # Update table header
  if (method %in% c("mean", "centile")) estlabel <- "**Adjusted Difference**" else estlabel <- "**Difference**"

  # Format estimate function

  tbl_results$table_header <-
    tibble(column = names(tbl_results$table_body)) %>%
    left_join(tbl_results$table_header, by = "column") %>%
    gtsummary:::table_header_fill_missing() %>%
    gtsummary:::table_header_fmt_fun(
      estimate = function(x) glue("{estimate_fun(x)}%"),
      p.value = pvalue_fun
    )

  tbl_results <-
    gtsummary:::modify_header_internal(
      tbl_results,
      estimate = estlabel,
      ci = "**95% CI**",
      p.value = "**p-value**"
    )

  # Update gt calls
  tbl_results <- gtsummary:::update_calls_from_table_header(tbl_results)

  # Add class
  class(tbl_results) <- c("tbl_propdiff", "gtsummary")

  return(tbl_results)
}
