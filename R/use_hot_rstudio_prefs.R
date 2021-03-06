#' Use H.O.T. RStudio Preferences
#'
#' @description
#' The function wraps `rstudio.prefs::use_rstudio_prefs()` and sets the following
#' preferences in RStudio.
#'
#' ```{r, echo = FALSE}
#' hot_prefs %>%
#'   purrr::map(as.character) %>%
#'   unlist() %>%
#'   tibble::enframe() %>%
#'   rlang::set_names(c("**Preference**", "**Value**")) %>%
#'   knitr::kable()
#' ```
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' use_hot_rstudio_prefs()
#' }}
use_hot_rstudio_prefs <- function() {
  # apply preferences ----------------------------------------------------------
  rstudio.prefs::use_rstudio_prefs(!!!hot_prefs)
}

# save preferences in list -----------------------------------------------------
hot_prefs <-
  list(always_save_history = FALSE,
       load_workspace = FALSE,
       margin_column = 80L,
       rainbow_parentheses = TRUE,
       restore_last_project = FALSE,
       rmd_chunk_output_inline = FALSE,
       show_hidden_files = TRUE,
       show_invisibles = TRUE,
       show_last_dot_value = TRUE,
       show_line_numbers = TRUE,
       show_margin = TRUE,
       save_workspace = "never")
