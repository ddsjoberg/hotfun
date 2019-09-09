#' Checks variable creation for new derived variables at once
#'
#' @param data data frame
#' @param checks list of variables to check.
#' @export
#' @examples
#' count_map(
#'   mtcars,
#'   list(c("cyl", "am"), c("gear", "carb"))
#' )

count_map <- function(data, checks) {
  # checking inputs
  stopifnot(is.data.frame(data))

  # if checks is a single vector, wrapping it in list
  if(rlang::is_bare_character(checks)) checks <- list(checks)

  # checking all variables are in data
  not_in_data <- unlist(checks) %>% setdiff(names(data))
  if (length(not_in_data) > 0) {
    stop(glue(
      "The following variables are not in 'data'\n",
      "{glue_collapse(not_in_data, sep = ', ')}"
    ))
  }

  # check that no names are ..n.. or ..p..
  if (any(c("..n..", "..p..") %in% unlist(checks))) {
    stop("'data' cannot have columns with names '..n..' or '..p..'")
  }

  # tabulating results
  walk(checks, ~ count_one(data, .x))

  invisible()
}

count_one <- function(data, vars) {

  # printing variable name being checked
  cat(vars[1], "\n")

  # printing unique obs
  data %>%
    count(!!!syms(vars), sort = TRUE, name = "..n..") %>%
    mutate(
      ..p.. = style_percent(.data$..n.. / sum(.data$..n..), symbol = TRUE)
    ) %>%
    as.data.frame() %>%
    print(., row.names = FALSE)

  # adding line break
  cat("\n")
}

