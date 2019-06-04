#' Apply variable labels to data frame
#'
#' Takes labels from the Derived Variables excel file and applies them to the
#' passed data frame.  The function is meant to be used in the pipe.
#'
#' @param data Data frame
#' @param path Path to Derived Variables xls/xlsx file
#' @param drop Logical indicating whether to drop unlabelled variables
#' @inheritParams readxl::read_excel
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' \donttest{
#' trial %>%
#'   set_derived_variables("derived_variables_sjoberg.xlsx")
#' }

set_derived_variables <- function(data, path, sheet = NULL, drop =  TRUE) {
  # import ---------------------------------------------------------------------
  # reading in excel file of Derived Variables
  df_derived_variables <- readxl::read_excel(path = path, sheet = sheet)

  # variable labels ------------------------------------------------------------
  # converting imported derived variables into named list with labels
  lst_variable_labels <-
    tibble(varname = names(data)) %>%
    inner_join(df_derived_variables, by = "varname") %>%
    select(.data$varname, .data$label) %>%
    spread(.data$varname, .data$label) %>%
    map(I)

  # applying the labels
  labelled::var_label(data) <- lst_variable_labels

  # drop -----------------------------------------------------------------------
  # dropping unlabelled data
  if (drop == TRUE) {
    data <-
      data %>%
      select(names(lst_variable_labels))
  }

  # return ---------------------------------------------------------------------
  # returning labelled data frame
  data
}
