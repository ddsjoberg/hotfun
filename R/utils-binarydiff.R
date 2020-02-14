
# Mode function
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# Calculate unadjusted proportions and difference in proportions
uv_binarydiff <- function(data, conflevel = 0.95) {

  # TODO: What about chi-squared warnings?
  uvtest <- stats::prop.test(table(data[["outcome_name"]], data[["predictor_name"]]), conf.level = conflevel)
  uvresults <-
    tibble(
      pred0 = uvtest$estimate[[1]],
      pred1 = uvtest$estimate[[2]],
      estimate = (pred0 - pred1),
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
      data %>% select(predictor_name) %>% unique()
    )

  return(newdata)

}
