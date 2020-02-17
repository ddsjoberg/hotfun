
# Mode function
# TODO: If more than one mode selected, how to determine?
# TODO: For now, use first one...
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  modes[1]
}

# Calculate unadjusted proportions and difference in proportions
uv_binarydiff <- function(data, conflevel = 0.95) {

  # TODO: What about chi-squared warnings?
  #uvtest <- stats::prop.test(table(data[["outcome_num"]], data[["predictor_num"]]),
                             #conf.level = conflevel, correct = FALSE)
  uvtest <- stats::prop.test(table(data[["predictor_num"]], data[["outcome_num"]]),
                             conf.level = conflevel, correct = FALSE)
  uvresults <-
    dplyr::tibble(
      pred0 = uvtest$estimate[[1]],
      pred1 = uvtest$estimate[[2]],
      estimate = (.data$pred0 - .data$pred1),
      conf.low = uvtest$conf.int[[1]],
      conf.high = uvtest$conf.int[[2]],
      p.value = uvtest$p.value
    )

  return(uvresults)

}

# Create new data using means/medians of selected covariates
create_newdata <- function(data, covariates) {

  newdata <-
  dplyr::bind_cols(
    data %>%
      dplyr::select(tidyselect::all_of(covariates)) %>%
      dplyr::summarize_if(is.numeric, mean, na.rm = TRUE),
    data %>%
      dplyr::select(tidyselect::all_of(covariates)) %>%
      dplyr::summarize_if(is.factor, Mode)
  ) %>%
    dplyr::mutate(freq = 2) %>%
    tidyr::uncount(freq) %>%
    dplyr::bind_cols(
      data %>% select(predictor_num) %>% unique()
    )

  return(newdata)

}

# Format results table
format_results <- function(results, column_labels, adjusted = FALSE) {

  if(adjusted == FALSE) estlabel <- "Difference"
  else if(adjusted == TRUE) estlabel <- "Adjusted Difference"

  tbl_formatted <-
    results %>%
    # Convert to 0-100 scale
    dplyr::mutate_at(vars(pred0, pred1, estimate, conf.low, conf.high), ~ . * 100) %>%
    gt::gt() %>%
    gt::cols_label(
      label = "",
      pred0 = md(glue::glue("**{column_labels[[1]]}** (N = {results$N0})")),
      pred1 = md(glue::glue("**{column_labels[[2]]}** (N = {results$N1})")),
      estimate = md(glue::glue("**{estlabel}**")),
      conf.low = md("**95% CI**"),
      p.value = md("**p-value**")
    ) %>%
    gt::cols_hide(vars(outcome, N, N0, N1)) %>%
    gt::fmt(vars(pred0, pred1, estimate, conf.low, conf.high), fns = style_sigfig) %>%
    gt::fmt(vars(p.value), fns = style_pvalue) %>%
    gt::cols_merge(vars(conf.low, conf.high), pattern = "{1}, {2}")

  return(tbl_formatted)

}
