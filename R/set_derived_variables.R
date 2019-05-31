#' Apply variable labels to data frame
#'
#' Takes labels from the Derived Variables excel file and applies them to the
#' passed data frame.  The function is meant to be used in the pipe.
#'
#' @param data data frame
#' @param path file path to the Derived Variables excel file
#' @param drop logical indicating whether to drop unlabelled variables
#' @author Daniel D. Sjoberg
#' @export

set_derived_variables <- function(data, path, drop =  TRUE) {
  # import ---------------------------------------------------------------------
  # reading in excel file of Derived Variables
  df_derived_variables <- readxl::read_excel(path)

  # factors --------------------------------------------------------------------
  # converting variables to factors
  fct_levels <-
    df_derived_variables %>%
    select(.data$varname, .data$levels) %>%
    spread(.data$varname, .data$levels) %>%
    imap(~eval(parse(text = .x))) %>%
    discard(~is.na(.) %>% all())

  # checking that the variable contains all levels specifed in orderd factor vec
  for (v in names(fct_levels)) {
    v_levels_not_specified <-
      stats::na.omit(data[[v]]) %>% setdiff(fct_levels[[v]])
    if (length(v_levels_not_specified) > 0) {
      stop(glue("Levels of column '{v}' not fully specified: ",
                "{paste(v_levels_not_specified, collapse = ', ')}"))
    }
  }

  # creating factor levels
  fcts <-
    fct_levels%>%
    imap(~ factor(data[[.y]], .x))

  # replacing character versions with the factors
  data[names(fcts)] <- fcts

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
