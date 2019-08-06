#' Assess  pattern of missing data
#'
#' Pass a data frame and the missing pattern of all columns in the data frame.
#' The data frame is returned unmodified.
#'
#' @param data data frame
#' @export
#' @examples
#' trial %>% count_na()

count_na <- function(data) {
  print("TRUE = 'Available', FALSE = 'Not Available'")
  data %>%
    mutate_all(~!is.na(.)) %>%
    dplyr::group_by_all() %>%
    dplyr::count() %>%
    print()

  invisible(data)
}

