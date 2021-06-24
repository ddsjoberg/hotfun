#' Use H.O.T. RStudio Preferences
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' use_hot_rstudio_prefs()
#' }}
use_hot_rstudio_prefs <- function() {
  # save preferences in list ---------------------------------------------------
  hot_prefs <-
    list(always_save_history = FALSE,
         load_workspace = FALSE,
         rainbow_parentheses = TRUE,
         restore_last_project = FALSE,
         save_workspace = "never",
         show_hidden_files = TRUE,
         show_invisibles = TRUE,
         show_last_dot_value = TRUE,
         margin_column = 80L,
         show_margin = TRUE,
         show_invisibles = TRUE,
         show_line_numbers = TRUE)

  # apply preferences ----------------------------------------------------------
  rstudio.prefs::use_rstudio_prefs(!!!hot_prefs)
}
