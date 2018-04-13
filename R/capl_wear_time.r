#' Calculates pedometer daily wear time.
#'
#' This function calculates pedometer daily wear time values and accounts for non-wear time values. 
#' @param time_on A character vector of time values.
#' @param time_off A character vector of time values.
#' @param non_wear_time An optional integer vector of time values in minutes. Default is 0.
#' @return A numeric vector of pedometer daily wear time values in decimal hours.
#' @export
#' @examples 
#' # Use of the function with time on, time off and non-wear time values
#' time_on <- c("07:15", "8:30 am", "10:00 AM", "11:30", "7am")
#' time_off <- c("6:30 pm", "20:59", "23:15", "10:30 PM", "9:45pm")
#' non_wear_time <- c(25, 60, 0, 0, 1)
#' capl_wear_time(time_on, time_off, non_wear_time)
#'
#' # Other examples
#' time_on <- c("07:15", "8:30 am", "10:00 AM", "11:30", "7am")
#' time_off <- c("20:59")
#' capl_wear_time(time_on, time_off)
#' 
#' capl_wear_time(time_on, time_off, 30)

capl_wear_time <- function(time_on, time_off, non_wear_time = 0) {
	loop <- function(time_on, time_off, non_wear_time) {
		time_on <- capl_time(time_on)
		time_off <- capl_time(time_off)
		non_wear_time <- as.integer(non_wear_time)
		if(is.na(non_wear_time)) non_wear_time <- 0
		if( sum(is.na(c(time_on, time_off)), na.rm = TRUE) > 0) {
			time_worn <- NA
		} else {
			time_difference <- lubridate::hm(time_off) - lubridate::hm(time_on)
			time_difference <- as.numeric(time_difference) / 3600
			if(time_difference < 0) {
				time_difference <- time_difference + 24
			} 
			time_difference <- time_difference - (non_wear_time / 60)
			if(time_difference < 0) time_worn <- NA else time_worn <- round(time_difference, 1)
		}
	}
	apply(X = data.frame(x = as.character(time_on), y = as.character(time_off), z = as.integer(non_wear_time)), MARGIN = 1, FUN = function(x) loop(x[1], x[2], x[3]))
}