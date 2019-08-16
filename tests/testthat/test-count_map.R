context("count_map")

test_that("No Errors/Warning with standard use", {
  expect_error(
    trial %>% count_map(list(c("stage", "grade"), c("grade", "response"))),
    NA
  )
  expect_warning(
    trial %>% count_map(list(c("stage", "grade"), c("grade", "response"))),
    NA
  )
})
