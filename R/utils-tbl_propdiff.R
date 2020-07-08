
# Calculate unadjusted proportions and difference in proportions - chisq p value (chisq.test and prop.test same)
calculate_unadjusted <- function(data, method = c("chisq", "exact"), x, y, conf.level = 0.95) {

  # Regardless of method, unadjusted difference gives normal approximation confidence interval
  chisqtest <- stats::prop.test(table(data[[x]], data[[y]]),
                                conf.level = conf.level, correct = FALSE)

  df_chisqtest <-
    tibble(
      pred0 = chisqtest$estimate[[1]],
      pred1 = chisqtest$estimate[[2]],
      estimate_2 = (.data$pred1 - .data$pred0),
      # Confidence intervals need to be flipped to match difference
      # Difference calculated to match tbl_ancova
      conf.low_2 = chisqtest$conf.int[[2]] * -1,
      conf.high_2 = chisqtest$conf.int[[1]] * -1,
    ) %>%
    select(-.data$pred0, -.data$pred1)

  # If chisq specified, use p-value from prop.test, otherwise use fisher.test
  if (method == "chisq") {
    df_unadjusted <-
      df_chisqtest %>%
      mutate(
        p.value_2 = chisqtest$p.value
      )
  } else if (method == "exact") {
    fishertest <- stats::fisher.test(table(data[[x]], data[[y]]))
    df_unadjusted <-
      df_chisqtest %>%
      mutate(
        p.value_2 = fishertest$p.value
      )
  }

  return(df_unadjusted)
}

# Function to create models and do predictions
# Have the option to return p-value so we don't have to build model again to get this
create_model_pred <- function(data, y, x, covariates, pvalue = FALSE) {

  # Save out formula for model
  model_formula <-
    glue(
      "{y} ~ {x} + ",
      glue_collapse(covariates, sep = " + ")
    ) %>%
    as.character()

  # Create model (reverse factor levels for consistency with tbl_ancova)
  model_obj <-
    stats::glm(
      stats::as.formula(model_formula),
      data = data,
      family = "binomial"
    )

  # Create new data using means/modes of selected covariates
  df_newdata <-
    bind_cols(
      data %>%
        select(tidyselect::all_of(covariates)) %>%
        summarize_if(is.numeric, mean, na.rm = TRUE),
      data %>%
        select(tidyselect::all_of(covariates)) %>%
        summarize_if(is.factor, get_mode, quiet = TRUE)
    ) %>%
    bind_cols(
      data %>%
        select(tidyselect::all_of(covariates)) %>%
        summarize_if(is.character, get_mode, quiet = TRUE)
    ) %>%
    mutate(freq = 2) %>%
    uncount(.data$freq) %>%
    bind_cols(
      data %>% select(tidyselect::all_of(x)) %>% unique()
    )

  # Adjusted probabilities and differences
  df_prediction <-
    broom::augment(
      model_obj,
      newdata = df_newdata,
      type.predict = "response"
    ) %>%
    # Reshape
    select(tidyselect::all_of(x), tidyselect::all_of(covariates),
      pred = .data$.fitted
    ) %>%
    mutate(reshapen = 1:dplyr::n()) %>%
    select(-tidyselect::all_of(x)) %>%
    tidyr::pivot_wider(
      names_from = .data$reshapen,
      names_prefix = "mv_pred",
      values_from = "pred"
    ) %>%
    # This matches tbl_ancova
    mutate(estimate_2 = (.data$mv_pred1 - .data$mv_pred2))

  # If pvalue = TRUE
  if (pvalue == TRUE) {

    # Add pvalue from adjusted model

    tidy_model_obj <- broom::tidy(model_obj)
    df_prediction <-
      df_prediction %>%
      mutate(p.value_2 = tidy_model_obj$p.value[2])
  }

  # Return predicted probabilities
  return(df_prediction)
}
