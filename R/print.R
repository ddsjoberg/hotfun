#' print and knit_print methods for hotfun objects
#'
#' @name print_hotfun
#' @param x an object created using hotfun functions
#' @param ... not used
#' @author Daniel D. Sjoberg
NULL

#' @rdname print_hotfun
#' @export
print.tbl_ancova <- function(x, ...) as_gt(x) %>% print()
