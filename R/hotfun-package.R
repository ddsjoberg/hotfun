#' @keywords internal
#' @import gtsummary
#' @import dplyr
#' @import purrr
#' @importFrom tidyr nest unnest complete spread
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data %||% set_names sym expr enexpr quo enquo parse_expr
#' @importFrom glue glue glue_collapse
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")

