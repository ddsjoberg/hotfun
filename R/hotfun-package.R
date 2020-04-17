#' @keywords internal
#' @importFrom gtsummary style_percent style_sigfig style_pvalue
#' @importFrom dplyr mutate select rename mutate_all between inner_join
#' count mutate_at bind_cols bind_rows left_join summarize_if slice filter
#' pull coalesce
#' @importFrom purrr map pmap map_dbl walk compact pluck map_chr pmap_int
#' map2 map_lgl
#' @importFrom tidyr nest unnest complete spread gather uncount
#' @importFrom glue glue glue_collapse
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym syms expr enexpr quo enquo
#' parse_expr := set_names
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
