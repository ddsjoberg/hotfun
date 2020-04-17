#' Assign a timepoint to a long dataset with multiple measures
#'
#' Given a data set that has a measure collected over time and you want to extract,
#' for example the 3 month measurement, this function will find the measure
#' closest to 3 months within a defined window.
#'
#' @param data data frame
#' @param id id variable name, such as `"mrn"`
#' @param ref_date baseline or reference date column name
#' @param measure_date date the measure was collected
#' @param timepoints vector of timepoint to identify
#' @param windows list of windows around a timepoint that are acceptable
#' @param time_units one of `c("days", "weeks", "months", "years")`
#' @param new_var name of new variable, default is `"timepoint"`
#'
#' @importFrom lubridate %--%
#' @export
#' @return data frame passed in `data` with additional column `new_var`
#' @examples
#' ggplot2::economics_long %>%
#'   dplyr::group_by(variable) %>%
#'   dplyr::mutate(min_date = min(date)) %>%
#'   dplyr::ungroup() %>%
#'   assign_timepoint(
#'     id = "variable",
#'     ref_date = "min_date",
#'     measure_date = "date",
#'     timepoints = c(6, 12, 24),
#'     windows = list(c(-2, 2), c(-2, 2), c(-2, 2)),
#'     time_units = "months"
#'   ) %>%
#'   dplyr::filter(!is.na(timepoint))
assign_timepoint <- function(data, id, ref_date, measure_date, timepoints, windows,
                             time_units = c("days", "weeks", "months", "years"),
                             new_var = "timepoint") {
  time_units <- match.arg(time_units)

  # checking for duplicates within id and ref_date
  if (duplicated(data[c(id, measure_date)]) %>% sum() > 0) {
    warning("`data` is not unique within `id` and `measure_date`. Results may differ depending on order of `data`.")
  }

  # assigning a function to calculate difference between dates
  time_diff_fun <- switch(
    time_units,
    "days" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::ddays(1),
    "weeks" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::dweeks(1),
    "months" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::dyears(1) * 12,
    "years" = function(x, y) lubridate::as.duration(x %--% y) / lubridate::dyears(1),
  )
  data <- data[c(id, ref_date, measure_date)]
  data[["..time_diff.."]] <-
    time_diff_fun(data[[ref_date]], data[[measure_date]])
  data[["..timepoint.."]] <- NA_real_

  # cycling through each timepoint and assigning the timepoint
  for (i in seq_len(length(timepoints))) {
    data <-
      data %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::arrange(abs(.data$..time_diff.. - timepoints[i])) %>%
      dplyr::mutate(
        ..timepoint.. = dplyr::if_else(
          between(
            .data$..time_diff..,
            timepoints[i] + windows[[i]][1],
            timepoints[i] + windows[[i]][2]
          ) &
            dplyr::row_number() == 1,
          timepoints[i],
          .data$..timepoint..
        )
      )
  }


  # returning data frame
  data %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data[[id]], .data[["..time_diff.."]]) %>%
    dplyr::select(c(id, ref_date, measure_date), .data$..timepoint..) %>%
    rlang::set_names(c(id, ref_date, measure_date, new_var))
}
