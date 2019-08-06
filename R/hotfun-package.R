#' @keywords internal
#' @import gtsummary
#' @import gt
#' @import dplyr
#' @import purrr
#' @importFrom tidyr nest unnest complete spread gather
#' @importFrom glue glue glue_collapse
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym syms expr enexpr quo enquo parse_expr
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")
