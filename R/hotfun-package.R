#' @keywords internal
#' @importFrom gtsummary style_percent style_sigfig style_pvalue tbl_summary add_n modify_header tbl_stack
#' @importFrom dplyr mutate select rename mutate_all between inner_join count mutate_at left_join bind_cols summarize summarize_if filter slice pull coalesce
#' @importFrom purrr map map2 pmap map_dbl walk compact map_chr pmap_int pluck map_lgl
#' @importFrom tidyr nest unnest complete spread gather uncount
#' @importFrom glue glue glue_collapse
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym syms expr enexpr quo enquo
#' parse_expr := set_names
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
