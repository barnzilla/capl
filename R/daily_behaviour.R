#' Compute pedometer wear time in decimal hours for a given day.
#'
#' @export
#'
#' @importFrom lubridate as.duration dminutes hm 
#' @importFrom stats var
#'
#' @param time_on a character element or vector representing the time (in 24-hour format) when the pedometer was first worn on a given day.
#' @param time_off a character element or vector representing the time (in 24-hour format) when the pedometer was removed at the end of a given day.
#' @param non_wear_time a numeric element or vector representing the total time (in minutes) the pedometer was not worn during waking hours on a given day.
#'
#' @examples
#' get_pedometer_wear_time(
#'   time_on = c("6:23", "5:50 am", NA),
#'   time_off = c("21:37", "9:17pm", ""),
#'   c(60, 90, 0)
#' )
#' # [1] 14.23 13.95    NA
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
get_pedometer_wear_time <- function(time_on = NA, time_off = NA, non_wear_time = NA) {
  try(
    if(var(c(length(time_on), length(time_off), length(non_wear_time))) == 0) {
      return(
        unname(
          apply(data.frame(time_on, time_off, non_wear_time), 1, function(x) {
            time_on <- suppressWarnings(hm(get_24_hour_clock(x[1])))
            time_off <- suppressWarnings(hm(get_24_hour_clock(x[2])))
            non_wear_time <- validate_number(x[3])
            if(is.na(non_wear_time) | non_wear_time < 0) {
              non_wear_time <- dminutes(0)
            } else {
              non_wear_time <- dminutes(non_wear_time)
            }
            if(sum(is.na(c(time_on, time_off, non_wear_time))) > 0) {
              NA
            } else {
              wear_time <- as.duration(time_off - time_on) - non_wear_time
              wear_time <- strsplit(as.character(wear_time), "~| |)")
              time_unit <- wear_time[[1]][4]
              wear_time <- validate_number(wear_time[[1]][3])
              if("minutes" %in% time_unit) {
                wear_time <- wear_time / 60
              }
              if(wear_time < 0) {
                NA
              } else {
                wear_time
              }
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the time_on, time_off and non_wear_time arguments must be the same length.")
    }
  )
}

#' Sum the number of valid days that a pedometer was worn.
#'
#' @export
#'
#' @param x a data frame or tibble with seven columns where each column represents a day of the week when a pedometer was worn and contains "yes" or 
#' "no" values (see [validate_steps()]).
#'
#' @examples
#' x <- data.frame(
#'   valid_steps1 = c("yes", "no", "yes"),
#'   valid_steps2 = c("no", "no", "yes"),
#'   valid_steps3 = c("yes", "yes", "yes"),
#'   valid_steps4 = c(NA, "no", "yes"),
#'   valid_steps5 = c("yes", "", "yes"),
#'   valid_steps6 = c("no", "yes", "yes"),
#'   valid_steps7 = c("yes", "yes", "no")
#' )
#'
#' get_valid_days(x)
#' # [1] 4 3 6
#'
#' @return returns a numeric element (if valid) or 0 (if not valid).
get_valid_days <- function(x = NULL) {
  try(
    if(is.null(x)) {
      stop("[CAPL error]: the x argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(x))) {
      stop("[CAPL error]: the x argument must be a data frame or a tibble.")
    } else {
      x <- apply(x, 2, validate_character)
      apply(x, 1, function(x) sum(x == "yes", na.rm = TRUE))
    }
  )
}

#' Check whether daily steps as measured by a pedometer are valid.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param steps a numeric element or vector representing the steps taken on a given day (valid values are between 1000 and 30000).
#' @param wear_time a numeric element representing the duration of time (in decimal hours) that a pedometer was worn on a given day (valid values are >= 10.0
#' hours).
#'
#' @examples
#' validate_steps(
#'   steps = c(5400, 11001, 999, 31000, 8796),
#'   wear_time = c(10.1, 12.6, 10.2, 10.9, 9.5)
#' )
#' # [1] "yes" "yes" "no"  "no"  "no" 
#'
#' @return returns "yes" (if valid) or "no" (if not valid).
validate_steps <- function(steps = NA, wear_time = NA) {
  try(
    if(var(c(length(steps), length(wear_time))) == 0) {
      return(
        unname(
          apply(data.frame(steps, wear_time), 1, function(x) {
            steps <- validate_number(x[1])
            wear_time <- validate_number(x[2])
            if(sum(is.na(c(steps, wear_time))) > 0) {
              "no"
            } else if(steps < 1000 | steps > 30000 | wear_time < 10) {
              "no"
            } else {
              "yes"
            }
          })
        )
      )
    } else {
      stop(stop("[CAPL error]: the steps and wear_time arguments must be the same length."))
    }
  )
}