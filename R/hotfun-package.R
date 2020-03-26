#' @keywords internal
#' @importFrom gtsummary style_percent
#' @importFrom dplyr mutate select rename mutate_all between inner_join
#' count
#' @importFrom purrr map pmap map_dbl walk compact
#' @importFrom tidyr nest unnest complete spread gather
#' @importFrom glue glue glue_collapse
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym syms expr enexpr quo enquo
#' parse_expr := set_names
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
