#' Results from a simulated study of two chemotherapy agents: Drug A and Drug B
#'
#' A dataset containing the baseline characteristics of 200 patients
#' who received Drug A or Drug B.  Dataset also contains the outcome of
#' tumor response to the treatment.
#'
#' @format A data frame with 200 rows--one row per patient
#' \describe{
#'     \item{trt}{Chemotherapy Treatment}
#'     \item{age}{Age, yrs}
#'     \item{marker}{Marker Level, ng/mL}
#'     \item{stage}{T Stage}
#'     \item{grade}{Grade}
#'     \item{response}{Tumor Response}
#'     \item{death}{Patient Died}
#'     \item{ttdeath}{Months to Death/Censor}
#' }
"trial"


#' H.O.T. project template
#'
#' The `project_template` object defines the contents of the H.O.T. project
#' template used in `create_hot_project()` and `use_hot_file()`.
#'
#' @format A quoted list defining the H.O.T. project template. Each item of
#' the list identifies one script or document that appears in the project template.
#' @examples
#' \donttest{\dontrun{
#' create_hot_project(
#'   path = file.path(tempdir(), "Sjoberg New Project"),
#'   template = hotfun::project_template
#' )
#' }}
#' @seealso [create_hot_project()]
#' @seealso [use_hot_file()]
"project_template"
