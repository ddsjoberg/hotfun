#' Table difference between two groups
#'
#' Provided a [stats::lm] object, a table of adjusted differences are returned.
#' @param x string indicating the binary comparison variable
#' @inheritParams gtsummary::tbl_uvregression
#' @author Daniel D. Sjoberg
#' @examples
#' trial %>%
#'   tbl_ancova(y = c("age", "marker"), x = "trt")
#' @export

tbl_ancova <- function(data, y, x, formula = "{y} ~ {x}", label = NULL,
                       method.args = NULL, conf.level = 0.95,
                       hide_n = FALSE, estimate_fun = style_sigfig,
                       pvalue_fun = style_pvalue, method = stats::lm) {
  # will return call, and all object passed to in tbl_regression call
  # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  # getting the by variable
  by <- stringr::word(x, sep = stringr::fixed("+")) %>% trimws()

  # checking the by variable has two levels
  if (data[[by]] %>% unique() %>% length() != 2) {
    stop(glue("The stratifying variable, '{by}', must have two levels."))
  }

  # building formula list
  formula_list <- formula <- map(y, function(y) glue(formula) %>% stats::as.formula())

  # building models
  models_list <-
    map(formula_list, ~ do.call(method, list(data = data, formula = .x, method.args)))

  # formatting model
  tbl_regression_list <- map(
    models_list,
    ~ tbl_regression(.x, conf.level = conf.level)
  )

  # creating list of variables that were adjusted for
  tbl_regression_list[[1]]  %>%
    pluck("table_body") %>%
    filter(!.data$variable %in% c(y, by)) %>%
    filter(.data$row_type == "label") %>%
    pull(.data$label)

  # putting label on all rows, and extracting one single row
  differences <- map2_dfr(
    tbl_regression_list, y,
    ~ .x %>%
      pluck("table_body") %>%
      filter(.data$variable == by) %>%
      arrange(.data$estimate) %>%
      slice(1) %>%
      mutate(variable = .y) %>%
      select(c("variable", "N", "estimate", "conf.low", "conf.high", "p.value"))
  )

  # univaraite summary statistics
  tbl_summary <-
    data %>%
    select(c(y, by)) %>%
    tbl_summary(
      by = by,
      statistic = list(..continuous.. = "{mean} ({sd})"),
      missing = "no"
    )

  # merging summary stats and differences
  table_body <-
    tbl_summary %>%
    pluck("table_body") %>%
    select(-.data$row_type) %>%
    left_join(
      differences,
      by = "variable"
    ) %>%
    select(c("variable", "label", "N", "stat_2", "stat_1", everything()))

  # returning results
  results <- list(
    table_body = table_body,
    inputs = func_inputs,
    gt_calls = eval(gt_tbl_ancova) %>%
      c(list(cols_label_by = tbl_summary$gt_calls$cols_label_by))
  )

  # assigning a class of tbl_ancova (for special printing)
  class(results) <- "tbl_ancova"
  return(results)
}



gt_tbl_ancova <- quote(list(
  # first call to the gt function
  gt = "gt(data = x$table_body)" %>%
    glue(),

  # alignment
  cols_align =
    "cols_align(align = 'center') %>% cols_align(align = 'left', columns = vars(label))" %>%
    glue(),

  # columns to hide
  cols_hide = "cols_hide(columns = vars(variable))" %>% glue(),

  # missing values
  fmt_missing = "fmt_missing(columns = everything(), missing_text = '---')" %>% glue(),

  # column headers
  cols_label = glue(
    "cols_label(",
    "N = md('**N**'), ",
    "label = md('**Characteristic**'), ",
    "estimate = md('**Difference**'), ",
    "conf.low = md('**{style_percent(conf.level, symbol = TRUE)} CI**'), ",
    "p.value = md('**p-value**')",
    ")"
  ),

  # adding p-value formatting (evaluate the expression with eval() function)
  fmt_pvalue =
    "fmt(columns = vars(p.value), rows = !is.na(p.value), fns = x$inputs$pvalue_fun)" %>%
    glue(),

  # ceof and confidence interval formatting
  fmt_estimate =
    "fmt(columns = vars(estimate, conf.low, conf.high), rows = !is.na(estimate), fns = x$inputs$estimate_fun)" %>%
    glue(),

  # combining conf.low and conf.high to print confidence interval
  cols_merge_ci =
    "cols_merge(col_1 = vars(conf.low), col_2 = vars(conf.high), pattern = '{1}, {2}')" %>%
    glue::as_glue(),

  # mean (sd) footnote
  footnote_summary_stat = glue(
    "tab_footnote(",
    "footnote = 'Mean (Standard Deviation)',",
    "locations = cells_column_labels(",
    "columns = vars(stat_1, stat_2))",
    ")"
  ),

  # column headers abbreviations footnote
  footnote_abbreviation = glue(
    "tab_footnote(",
    "footnote = 'CI = Confidence Interval',",
    "locations = cells_column_labels(",
    "columns = vars(conf.low))",
    ")"
  )

))






