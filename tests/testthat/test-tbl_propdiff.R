context("tbl_propdiff")

set.seed(6038503)

test_that("No errors/warnings with standard use", {

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = c("response", "death"), predictor = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 method = "centile", covariates = c("age", "stage"), bootstrapn = 50),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
               method = "mean", covariates = c("age", "stage"), bootstrapn = 50),
    NA)

  expect_warning(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt"),
    NA
  )

  expect_warning(
    tbl_propdiff(data = trial, outcome = c("response", "death"), predictor = "trt",
                 method = "centile", covariates = c("age", "stage"), bootstrapn = 50),
    NA
  )

  expect_warning(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 method = "mean", covariates = c("age", "stage"), bootstrapn = 50),
    NA)

})

test_that("Expect messages if `method` and/or `bootstrapn` not specified", {

  expect_message(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 covariates = c("age", "stage"), bootstrapn = 50),
    "*"
  )

  expect_message(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 covariates = c("age", "stage"), method = "mean"),
    "*"
  )

})

test_that("No messages if `method` and `bootstrapn` specified", {

  expect_message(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 covariates = c("age", "stage"), method = "mean", bootstrapn = 50),
    NA
  )

})

test_that("Error if variables do not exist or temporary variables do exist", {

  expect_error(
    tbl_propdiff(trial, outcome = "response", predictor = "trt_new"),
    "*"
  )

  expect_error(
    tbl_propdiff(trial, outcome = "response_new", predictor = "trt"),
    "*"
  )

  expect_error(
    tbl_propdiff(trial %>% rename(..outnum.. = response),
                 outcome = "..outnum..", predictor = "trt"),
    "*"
  )

})

test_that("Error if `conflevel` outside of 0-1 range", {

  expect_error(
    tbl_propdiff(trial, outcome = "response", predictor = "trt", conflevel = 95),
    "*"
  )

})

test_that("Error if outcome or predictor has more or less than 2 non-missing levels", {

  expect_error(
    tbl_propdiff(data = trial %>% mutate(response2 = sample(c(0:2), size = nrow(trial), replace = TRUE)),
                 outcome = "response2", predictor = "trt"),
    "*"
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt2 = sample(c(0:2), size = nrow(trial), replace = TRUE)),
                 outcome = "response", predictor = "trt2"),
    "*"
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt2 = 0), outcome = "response", predictor = "trt2"),
    "*"
  )

})

test_that("Predictor and outcome can be character, numeric or factor", {

  expect_error(
    tbl_propdiff(data = trial %>% mutate(response = as.character(response)),
                 outcome = "response", predictor = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(response = as.character(response)),
                 outcome = "response", predictor = "trt",
                 covariates = c("age"), method = "mean", bootstrapn = 50),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt = as.numeric(as.factor(trt))),
                 outcome = "response", predictor = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt = as.numeric(as.factor(trt))),
                 outcome = "response", predictor = "trt",
                 covariates = c("age"), method = "mean", bootstrapn = 50),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt = as.factor(trt), response = as.factor(response)),
                 outcome = "response", predictor = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt = as.factor(trt), response = as.factor(response)),
                 outcome = "response", predictor = "trt",
                 covariates = c("age"), method = "mean", bootstrapn = 50),
    NA
  )

})

test_that("`rev` option runs without error", {

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt", rev = TRUE),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt", covariates = c("age", "stage"),
                 method = "centile", rev = TRUE, bootstrapn = 50),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt", covariates = c("age", "stage"),
                 method = "mean", rev = TRUE, bootstrapn = 50),
    NA
  )

})

test_that("No errors if outcome variable does not have a label", {

  expect_error(
    tbl_propdiff(data = trial %>% mutate(response2 = sample(c(0, 1), size = nrow(trial), replace = TRUE)),
                 outcome = "response2", predictor = "trt"),
    NA
  )

})

test_that("No errors when using different confidence levels", {

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt", conflevel = 0.90),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt", covariates = c("age", "stage"),
                 method = "centile", conflevel = 0.90, bootstrapn = 50),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt", covariates = c("age", "stage"),
                 method = "mean", conflevel = 0.90, bootstrapn = 50),
    NA
  )

})

test_that("`estimate_fun` and `pvalue_fun` are functions", {

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 estimate_fun = "style_percent"),
    "*"
  )

  expect_error(
    tbl_propdiff(data = trial, outcome = "response", predictor = "trt",
                 pvalue_fun = "style_pvalue"),
    "*"
  )

})
