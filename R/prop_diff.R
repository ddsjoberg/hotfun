

prop_diff <- function(data, variable, by, adj.vars, conf.level = 0.95,
                      method = c("centile", "sd"), boot_n = 1000, ...) {

}


logistic_difference_estimate <- function(data, variable, by, adj.vars) {
  browser()
  data <-
    data %>%
    select(all_of(c(by, variable, adj.vars))) %>%
    filter(stats::complete.cases(.))

  model_matrix <-
    paste(c(variable, adj.vars), collapse = " + ") %>%
    {paste0("~", .)} %>%
    as.formula() %>%
    {model.matrix(., data = data)[, -1]}

  mod <- glm(model.matrix(~data[[by]])[, -1] ~ model_matrix, family = stats::binomial)

  df_covar_means <-
    model_matrix %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(vars(-1), mean)

  predict(mod, newdata = df_covar_means) %>%
    dplyr::select(1, last_col()) %>%
    dplyr::distinct()


}

logistic_difference_estimate(gtsummary::trial, "response", "trt", c("age", "grade"))
#
# glm(trt ~ age + grade, gtsummary::trial)
# model.matrix(response ~ trt + age + grade, gtsummary::trial)[ , -1]

