#' Calculate an AUC from a histogram
#'
#' Uses a histogram of event probabilties to calculate a precise AUC.  This is
#' a discrete approximation.  Use this function with many break points with a
#' large number of data points.
#'
#' @param x histogram object from [graphics::hist]
#' @author Daniel D. Sjoberg
#' @export
#' @examples
#' runif(10000) %>%
#'   hist(breaks = 250) %>%
#'   auc_histogram()

auc_histogram <- function(x){

  # calculating mean
  mu = weighted.mean(x$mids, x$density)

  x_length = length(x$count)
  x_width = x$breaks[2:(x_length + 1)] - x$breaks[1:x_length]

  # calculating sensitivity and specificity
  sens_spec <-
    purrr::map_dfr(
      1:x_length,
      function(i) {
        # calculating Sens and Spec using Bayes Rule
        sens = sum(x$mids[i:x_length] * x$density[i:x_length] * x_width[i:x_length]) / mu
        spec = ( sum(x$density[1:i] * x_width[1:i]) -
                   sum(x$mids[1:i] * x$density[1:i] * x_width[1:i]) ) / (1 - mu)
        tibble::tibble(x = x$mids[i], sensitivity = sens, specificity = spec)
      }
    )

  # calculating the AUC (using the trapezoidal rule)
  idx = 2:x_length
  auc = -(sens_spec$specificity[idx-1] - sens_spec$specificity[idx]) %*%
    (sens_spec$sensitivity[idx] + sens_spec$sensitivity[idx-1]) / 2


  auc %>% as.vector()
}

