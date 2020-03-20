
# Calculate unadjusted proportions and difference in proportions
calculate_uvpropdiff <- function(data, x, y, conflevel = 0.95) {

  uvtest <- stats::prop.test(table(data[[x]], data[[y]]),
                             conf.level = conflevel, correct = FALSE)
  uvresults <-
    tibble(
      pred0 = uvtest$estimate[[1]],
      pred1 = uvtest$estimate[[2]],
      estimate = (.data$pred0 - .data$pred1),
      conf.low = uvtest$conf.int[[1]],
      conf.high = uvtest$conf.int[[2]],
      p.value = uvtest$p.value
    ) %>%
    select(-.data$pred0, -.data$pred1)

  return(uvresults)

}

# Function to create models and do predictions
# Have the option to return p-value so we don't have to build model again to get this
create_model_pred <- function(data, y, x, covariates, pvalue = FALSE) {

  # Save out formula for model
  model_formula <-
    glue("{y} ~ {x} + ",
         glue_collapse(covariates, sep = " + ")) %>%
    as.character()

  # Create model
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
           pred = .data$.fitted) %>%
    tidyr::pivot_wider(
      names_from = tidyselect::all_of(x),
      names_prefix = "mv_pred",
      values_from = "pred"
    ) %>%
    mutate(estimate = (.data$mv_pred0 - .data$mv_pred1))

  # If pvalue = TRUE
  if(pvalue == TRUE) {

    # Add pvalue from adjusted model

    tidy_model_obj <- broom::tidy(model_obj)
    df_prediction <-
      df_prediction %>%
      mutate(p.value = tidy_model_obj[tidy_model_obj$term == x, ][["p.value"]])
  }

  # Return predicted probabilities
  return(df_prediction)

}
