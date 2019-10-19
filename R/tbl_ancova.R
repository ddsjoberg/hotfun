#' Table difference between two groups
#'
#' Provided a [stats::lm] object, a table of adjusted differences are returned.
#' @param y vector of continuous outcome variables.  One ANOVA/ANCOVA model
#' will be computed for each outcome.  All results returned in a single table.
#' @param x string indicating the binary comparison variable
#' @param label Named list of labels
#' @inheritParams gtsummary::tbl_uvregression
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_ancova(trial, y = c("age", "marker"), x = "trt")
#' @export

tbl_ancova <- function(data, y, x, formula = "{y} ~ {x}", label = NULL,
                       method.args = NULL, conf.level = 0.95,
                       hide_n = FALSE, estimate_fun = style_sigfig,
                       pvalue_fun = style_pvalue, method = stats::lm) {
  # # will return call, and all object passed to in tbl_regression call
  # # the object func_inputs is a list of every object passed to the function
  func_inputs <- as.list(environment())

  # checking inputs ------------------------------------------------------------
  if (!rlang::is_string(x)) {
    rlang::abort("`x` must be a string of length one.")
  }

  if (!rlang::is_character(y)) {
    rlang::abort("`y` must be a character vector.")
  }

  # calculate summaries --------------------------------------------------------
  results <-
    tibble(
      outcome = y,
      outcome_label = map(
        .data$outcome,
        ~ label[[.x]] %||% attr(data[[.x]], "label") %||% .x
      ),
      formula = map(.data$outcome, function(y) glue(formula) %>% stats::as.formula()),
      model = map(
        .data$formula,
        ~ do.call(method, list(data = data, formula = .x, method.args))
      ),
      tbl_regression = pmap(
        list(.data$model, .data$outcome_label, .data$outcome),
        function(.x, .y, .z) {
          tbl <-
            gtsummary::tbl_regression(
              .x, show_single_row = x, conf.level = conf.level,
              estimate_fun = style_sigfig, pvalue_fun = style_pvalue,
              include = x
            ) %>%
            gtsummary::modify_header(estimate = "**Difference**")
          tbl$table_body$label = .y
          tbl$table_body$variable = .z
          tbl
        }
      ),
      tbl_summary = pmap(
        list(.data$model, .data$outcome_label, .data$outcome),
        function(.x, .y, .z) {
          df <- stats::model.frame(.x)[c(.z, x)]
          tbl <- gtsummary::tbl_summary_(df, by = x, missing = "no") %>%
            gtsummary::modify_header(stat_by = "**{level}**")
          if (hide_n == FALSE) tbl <- gtsummary::add_n(tbl)
          tbl$table_body$label = .y
          stat_cols <- names(tbl$table_body)[startsWith(names(tbl$table_body), "stat_")]
          tbl$table_body[,stat_cols] <- tbl$table_body[,rev(stat_cols)]
          tbl
        }
      ),
      tbl_merge = map2(
        .data$tbl_summary, .data$tbl_regression,
        ~tbl_merge(list(.x, .y))
      )
    )

  # patch together final table -------------------------------------------------
  if (nrow(results) > 1) tlb_stack <- gtsummary::tbl_stack(results$tbl_merge)
  else tlb_stack <- results$tbl_merge[[1]]
  tlb_stack$gt_calls$tab_spanner <- NULL

  tlb_stack
}
