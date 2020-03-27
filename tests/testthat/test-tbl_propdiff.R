context("tbl_propdiff")

set.seed(6038503)

test_that("No errors/warnings with standard use", {
  expect_error(
    tbl_propdiff(data = trial, y = "response", x = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(data = trial, y = c("response", "death"), x = "trt"),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial, y = "response", x = "trt", formula = "{y} ~ {x} + age + stage",
      method = "boot_centile", bootstrapn = 50
    ),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial, y = "response", x = "trt", formula = "{y} ~ {x} + age + stage",
      method = "boot_sd", bootstrapn = 50
    ),
    NA
  )
})

test_that("Error if variables do not exist or temporary variables do exist", {
  expect_error(
    tbl_propdiff(trial, y = "response", x = "trt_new"),
    "*"
  )

  expect_error(
    tbl_propdiff(trial, y = "response_new", x = "trt"),
    "*"
  )
})

test_that("Error if `conflevel` outside of 0-1 range", {
  expect_error(
    tbl_propdiff(trial, y = "response", x = "trt", conf.level = 95),
    "*"
  )
})

test_that("Error if outcome or predictor has more or less than 2 non-missing levels", {
  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(response2 = sample(c(0:2), size = nrow(trial), replace = TRUE)),
      y = "response2", x = "trt"
    ),
    "*"
  )

  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(trt2 = sample(c(0:2), size = nrow(trial), replace = TRUE)),
      y = "response", x = "trt2"
    ),
    "*"
  )

  expect_error(
    tbl_propdiff(data = trial %>% mutate(trt2 = 0), y = "response", x = "trt2"),
    "*"
  )
})

test_that("x, y and covariates can be character, numeric or factor", {
  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(response = as.character(response)),
      y = "response", x = "trt"
    ),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(response = as.character(response)),
      y = "response", x = "trt", formula = "{y} ~ {x} + age", method = "boot_sd", bootstrapn = 50
    ),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(trt = as.numeric(as.factor(trt))),
      y = "response", x = "trt"
    ),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(trt = as.numeric(as.factor(trt))),
      y = "response", x = "trt", formula = "{y} ~ {x} + age", method = "boot_sd", bootstrapn = 50
    ),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(grade = as.character(grade)),
      y = "response", x = "trt", formula = "{y} ~ {x} + grade"
    ),
    NA
  )
})

test_that("No errors if outcome variable does not have a label", {
  expect_error(
    tbl_propdiff(
      data = trial %>% mutate(response2 = sample(c(0, 1), size = nrow(trial), replace = TRUE)),
      y = "response2", x = "trt"
    ),
    NA
  )
})

test_that("No errors when using different confidence levels", {
  expect_error(
    tbl_propdiff(data = trial, y = "response", x = "trt", conf.level = 0.90),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial, y = "response", x = "trt", formula = "{y} ~ {x} + age + stage",
      method = "boot_centile", conf.level = 0.90, bootstrapn = 50
    ),
    NA
  )

  expect_error(
    tbl_propdiff(
      data = trial, y = "response", x = "trt", formula = "{y} ~ {x} + age + stage",
      method = "boot_sd", conf.level = 0.90, bootstrapn = 50
    ),
    NA
  )
})

test_that("`estimate_fun` and `pvalue_fun` are functions", {
  expect_error(
    tbl_propdiff(
      data = trial, y = "response", x = "trt",
      estimate_fun = "style_percent"
    ),
    "*"
  )

  expect_error(
    tbl_propdiff(
      data = trial, y = "response", x = "trt",
      pvalue_fun = "style_pvalue"
    ),
    "*"
  )
})
