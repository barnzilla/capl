#' Compute pedometer wear time in decimal hours for a given day.
#'
#' @export
#'
#' @importFrom lubridate as.duration dminutes hm 
#'
#' @param time_on a character element or vector representing the time (in 24-hour format) when the pedometer was first worn on a given day.
#' @param time_off a character element or vector representing the time (in 24-hour format) when the pedometer was removed at the end of a given day.
#' @param non_wear_time a numeric element or vector representing the total time (in minutes) the pedometer was not worn during waking hours on a given day.
#'
#' @examples
#' get_pedometer_wear_time(
#'   time_on = "6:23", 
#'   time_off = "21:55", 
#'   non_wear_time = 90
#' )
#' # [1] 14.03
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
get_pedometer_wear_time <- function(time_on = NA, time_off = NA, non_wear_time = NA) {
  return(
    unname(
      apply(data.frame(time_on, time_off, non_wear_time), 1, function(x) {
        time_on <- suppressWarnings(hm(x[1]))
        time_off <- suppressWarnings(hm(x[2]))
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
}