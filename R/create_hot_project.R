#' Start a new H.O.T. project
#'
#' Creates a directory with the essential files for a new project.
#' The function can be used on existing project directories as well.
#' This is a thin wrapper for `starter::create_project()` that
#' sets the default template to `template = hotfun::project_template`
#'
#' @param template Specifies template for `starter::create_project(template=)`.
#' Default is `hotfun::project_template`
#' @inheritParams starter::create_project
#' @inheritDotParams starter::create_project
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' # specifying project folder location (folder does not yet exist)
#' project_path <- fs::path(tempdir(), "My Project Folder")
#'
#' # creating folder where secure data would be stored (typically will be a network drive)
#' secure_data_path <- fs::path(tempdir(), "secure_data")
#' dir.create(secure_data_path)
#'
#' # creating new project folder
#' create_hot_project(project_path, path_data = secure_data_path)
#' }}
create_hot_project <- function(path, path_data = NULL,
                               template = hotfun::project_template, ...) {

  starter::create_project(
    path = path,
    path_data = path_data,
    template = template,
    ...
  )
}
