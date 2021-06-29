#' Write a template file
#'
#' Rather than using `create_hot_project()` to start a new project folder, you
#' may use `use_hot_file()` to write a single file from any project template.
#' The functions `use_hot_gitignore()` and `use_hot_readme()` are shortcuts for
#' `use_hot_file("gitignore")` and `use_hot_file("readme")`.
#'
#' @inheritParams starter::use_project_file
#' @inheritParams create_hot_project
#' @name use_hot_file
#' @rdname use_hot_file
#' @seealso [`create_hot_project()`]
#' @export
#' @examples
#' \donttest{\dontrun{
#' # create gitignore file
#' use_project_file("gitignore")
#' use_project_gitignore()
#'
#' # create README.md file
#' use_project_file("readme")
#' use_project_readme()
#' }}

use_hot_file <- function(name = NULL, filename = NULL,
                         template = hotfun::project_template, open = interactive()) {
  starter::use_project_file(name = name, filename = filename,
                           template = template, open = open)
}

#' @rdname use_hot_file
#' @export
use_hot_gitignore <- function(filename = NULL, template = NULL) {
  use_hot_file(name = "gitignore", filename = filename,
               template = template)
}

#' @rdname use_hot_file
#' @export
use_hot_readme <- function(filename = NULL, template = NULL) {
  use_hot_file(name = "readme", filename = filename,
               template = template)
}

