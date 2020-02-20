#' @keywords internal
#' @import gtsummary
#' @import gt
#' @importFrom dplyr select filter rename mutate mutate_at mutate_all arrange pull slice summarize summarize_if inner_join left_join right_join full_join bind_rows bind_cols count between
#' @importFrom purrr map map2 pmap map_dbl map2_dbl pmap_dbl map_chr map2_chr pmap_chr map_int map2_int pmap_int pluck walk compact
#' @importFrom tidyr nest unnest complete spread gather uncount
#' @importFrom glue glue glue_collapse
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym syms expr enexpr quo enquo
#' parse_expr :=
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
