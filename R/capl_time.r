#' Converts CAPL pedometer time on/time off data to 24-hour format.
#'
#' This function converts time strings to 24-hour format (hh:mm). 
#' @param x A character vector of time values.
#' @return A character vector of time values in 24-hour format (hh:mm).
#' @export
#' @examples 
#' time_values <- c("4:15 PM", "8:00", "20:45", "7pm", "9:23am")
#' capl_time(time_values)

capl_time <- function(x) {
	loop <- function(x) {
		am_check <- length(grep("am", tolower(x)))
		pm_check <- length(grep("pm", tolower(x)))
		y <- strsplit(tolower(x), "\\, |\\,| ")
		z <- strsplit(y[[1]][1], ":")
		if(length(z[[1]]) >= 2) {
			h <- as.numeric(z[[1]][1])
			if(class(z[[1]][2]) == "character") m <- as.numeric(gsub("([0-9]+).*$", "\\1", z[[1]][2])) else m <- as.numeric(z[[1]][2])
		} else if(length(z[[1]]) == 1) {
			h <- as.numeric(gsub("([0-9]+).*$", "\\1", z))
			m <- 0
		} else { output <- NA }
		if(!is.na(h) & !is.na(m) & h <= 23 & m <= 59) {
			if(am_check > 0) {
				if(h > 12) {
					h <- NA
				} else if(h == 12) {
					 h <- "00"
				} else {
					if(h < 10) h <- paste0(0, h) 
				}
			} else if(pm_check > 0) { if(h < 12) h <- h + 12 } else { if(h < 10) h <- paste0(0, h) }
			if(is.na(h)) {
				output <- NA
			} else {
				m <- as.numeric(m)
				if(m < 10) m <- paste0(0, m)
				output <- paste(h, ":", m, sep = "")
			}
		} else { output <- NA }
	}
	sapply(x, function(x) loop(x), USE.NAMES = FALSE)
}