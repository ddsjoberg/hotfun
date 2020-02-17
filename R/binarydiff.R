library(tidyverse)
library(gtsummary)
set.seed(6034370)

# TODO: Checks
# Check it works with numeric, character or factor predictor or outcome
# Check "rev" option works properly
# Check with and without covariates
# Check it works with/without column labels
# Check it works with/without variable labels
# Check it works with different confidence levels

binarydiff <- function(data, outcome, predictor, covariates = NULL, rev = FALSE, conflevel = 0.95, bootstrapn = 2000) {

  ### CHECKS------------------

  #browser()

  # Confirm that both outcome and predictor exist
  if (dplyr::intersect(c(predictor, outcome), names(data)) %>% length() != 2) {
    stop(glue::glue("Both '{predictor}' and '{outcome}' must be variables in dataset."),
         call. = FALSE)
  }

  # Check that both outcome and predictor are binary
  if (data[!is.na(data[[predictor]]), ][[predictor]] %>% unique() %>% length() != 2) {
    stop(glue::glue("The stratifying variable, '{predictor}', must have two and only two non-missing levels."),
         call. = FALSE)
  }
  if (data[!is.na(data[[outcome]]), ][[outcome]] %>% unique() %>% length() != 2) {
    stop(glue::glue("The outcome variable, '{outcome}', must have two and only two non-missing levels."),
         call. = FALSE)
  }

  ### SETUP-------------------

  # Convert to strings
  predictor <- dplyr::select(data[0, ], {{ predictor }}) %>% names()
  outcome <- dplyr::select(data[0, ], {{ outcome }}) %>% names()

  # Create outcome label before it is stripped from data
  outcome_label <- attr(data[[outcome]], "label")

  # Convert outcomes to 0/1 numeric values and drop any NA outcome/predictor values
  data <-
    data[!is.na(data[[predictor]]) & !is.na(data[[outcome]]), ] %>%
    dplyr::mutate(
      predictor_num = dplyr::group_indices(., .data[[predictor]]),
      outcome_num = dplyr::group_indices(., .data[[outcome]])
    ) %>%
    dplyr::mutate_at(vars(.data$predictor_num, .data$outcome_num), ~ . - 1)

  # If reversing the groups/difference, flip values here
  if(rev == TRUE) {
    data <-
      data %>%
      dplyr::mutate(predictor_num =
                      dplyr::case_when(
                        .data$predictor_num == 0 ~ 1,
                        .data$predictor_num == 1 ~ 0
                      ))
  }

  # Pull out values from original predictor variable to use in table labels
  column_labels <- data %>% dplyr::select(predictor_num, predictor) %>%
    unique() %>% dplyr::arrange(predictor_num) %>% dplyr::pull(predictor)

  # For model, keep necessary variables and complete cases only
  data_model <-
    data %>%
    dplyr::select(.data$outcome_num, .data$predictor_num, tidyselect::all_of(covariates)) %>%
    dplyr::filter(stats::complete.cases(.) == TRUE)

  ### UNADJUSTED DIFFERENCE------------------

  uvresults <-
    dplyr::tibble(
      outcome = outcome,
      label = if_else(is.null(outcome_label), outcome, outcome_label)
    ) %>%
    dplyr::bind_cols(
      data_model %>%
        dplyr::count(.data$predictor_num) %>%
        tidyr::pivot_wider(names_from = "predictor_num", names_prefix = "N", values_from = "n")
    ) %>%
    dplyr::mutate(N = .data$N0 + .data$N1) %>%
    dplyr::bind_cols(
      uv_binarydiff(data_model, conflevel = conflevel)
    )

  # If no covariates specified, export unadjusted difference
  if(is.null(covariates)) {

    # Convert results to table and return
    tbl_uvresults <-
      format_results(uvresults, column_labels, adjusted = FALSE)
    return(tbl_uvresults)

  }

  ### ADJUSTED DIFFERENCE-----------------

  # Save out formula for model
  model_formula <-
    glue::glue("outcome_num ~ predictor_num + ",
               glue::glue_collapse(covariates, sep = " + ")) %>%
    as.character()

  # Create logistic regression model
  model_obj <-
    stats::glm(
      as.formula(model_formula),
      data = data_model,
      family = "binomial"
    )

  # New dataset with mean/mode of covariates
  df_newdata <- create_newdata(data_model, covariates)

  # Calculate adjusted probability and then difference between groups
  df_prediction <-
    broom::augment(
      model_obj,
      newdata = df_newdata,
      type.predict = "response"
    ) %>%
    # Reshape
    dplyr::select(.data$predictor_num, tidyselect::all_of(covariates), pred = .fitted) %>%
    tidyr::pivot_wider(
      names_from = "predictor_num",
      names_prefix = "pred",
      values_from = "pred"
    ) %>%
    dplyr::mutate(estimate = (.data$pred1 - .data$pred0))

  # pvalue from adjusted model
  pval_predictor <-
    broom::tidy(model_obj) %>%
    dplyr::filter(term == "predictor_num") %>%
    dplyr::pull(.data$p.value)

  # Create list of bootstrap indicators so we don't have to save out data 2000x
  bs_list <- purrr::map(seq_along(1:bootstrapn), ~ sample.int(nrow(data_model), replace = TRUE))

  # Create map dataframe for all datasets
  df_bs_map <-
    dplyr::tibble(idn = 1:bootstrapn) %>%
    dplyr::mutate(bs_assignment = purrr::map(idn, ~ bs_list[[..1]]))

  # Bootstrapping
  df_bs_results <-
    df_bs_map %>%
    mutate(
      # Create models
      model_obj_bs =
        purrr::pmap(
          list(model_formula, bs_assignment),
          ~ stats::glm(
            as.formula(..1),
            data = data_model %>% dplyr::slice(..2),
            family = "binomial"
          )
        ),
      df_newdata_bs =
        purrr::pmap(
          # Save out new data for predictions
          list(bs_assignment),
          ~ create_newdata(data_model %>% dplyr::slice(..1), covariates)
        ),
      # Predicted probabilities and differences
      df_prediction_bs =
        purrr::pmap(
          list(model_obj_bs, df_newdata_bs),
          ~ broom::augment(
            ..1,
            newdata = ..2,
            type.predict = "response"
          ) %>%
            # Reshape
            dplyr::select(.data$predictor_num, tidyselect::all_of(covariates), pred = .fitted) %>%
            tidyr::pivot_wider(
              names_from = "predictor_num",
              names_prefix = "pred",
              values_from = "pred"
            ) %>%
            dplyr::mutate(estimate = (.data$pred1 - .data$pred0))
        )
    )

  # Collapse predicted differences across all bootstraps
  df_ci_results <-
    df_bs_results %>%
    dplyr::select(.data$df_prediction_bs) %>%
    tidyr::unnest(.data$df_prediction_bs) %>%
    dplyr::summarize(
      conf.low = quantile(estimate, 0.025, na.rm = TRUE),
      conf.high = quantile(estimate, 0.975, na.rm = TRUE)
    )

  # Merge in with main results and format
  mvresults <-
    dplyr::bind_cols(
      uvresults %>% dplyr::select(-c(.data$estimate, .data$conf.low, .data$conf.high, .data$p.value)),
      df_prediction %>% dplyr::select(.data$estimate)
    ) %>%
    dplyr::bind_cols(df_ci_results) %>%
    dplyr::mutate(
      outcome = outcome,
      conf.low = conf.low,
      conf.high = conf.high,
      estimate = estimate,
      p.value = pval_predictor
    ) %>%
    dplyr::select(outcome, label, N0, N1, N, pred0, pred1, estimate, conf.low, conf.high, p.value)

  # Convert results to table and return
  tbl_mvresults <-
    format_results(mvresults, column_labels, adjusted = TRUE)

  return(tbl_mvresults)

}
