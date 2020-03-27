
# Calculate unadjusted proportions and difference in proportions
calculate_exact <- function(data, x, y, conf.level = 0.95) {
  exacttest <- stats::prop.test(table(data[[x]], data[[y]]),
    conf.level = conf.level, correct = FALSE
  )

  exacttest <-
    tibble(
      pred0 = exacttest$estimate[[1]],
      pred1 = exacttest$estimate[[2]],
      estimate_2 = (.data$pred1 - .data$pred0),
      # Confidence intervals need to be flipped to match difference
      # Difference calculated to match tbl_ancova
      conf.low_2 = exacttest$conf.int[[2]] * -1,
      conf.high_2 = exacttest$conf.int[[1]] * -1,
      p.value_2 = exacttest$p.value
    ) %>%
    select(-.data$pred0, -.data$pred1)

  return(exacttest)
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
      names_from = reshapen,
      names_prefix = "mv_pred",
      values_from = "pred"
    ) %>%
    # This matches tbl_ancova
    mutate(estimate_2 = (.data$mv_pred1 - .data$mv_pred2))

  # If pvalue = TRUE
  if (pvalue == TRUE) {

    # Add pvalue from adjusted model

    tidy_model_obj <- broom::tidy(model_obj)
    # TODO: Since names are different, is it okay to just use 2nd row?
    df_prediction <-
      df_prediction %>%
      mutate(p.value_2 = tidy_model_obj$p.value[2])
    # df_prediction <-
    # df_prediction %>%
    # mutate(p.value_2 = tidy_model_obj[tidy_model_obj$term == tidyselect::all_of(x), ][["p.value"]])
    # tidy_model_obj[tidy_model_obj$term == tidyselect::all_of(x), ]
  }

  # Return predicted probabilities
  return(df_prediction)
}
