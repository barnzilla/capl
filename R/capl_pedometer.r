#' Evaluates whether pedometer steps and wear time qualify as a valid day of pedometry and provides a CAPL pedometer score.
#'
#' This function evaluates whether pedometer steps and wear time qualify as a valid day of pedometry and provides a pedometer score based on the CAPL scoring system. The rules for a valid day of pedometry are: 1000 <= steps <= 30000 and wear_time >= 10 hours.
#' @param steps An integer vector of pedometer steps.
#' @param wear_time A numeric vector of pedometer wear time in decimal hours.
#' @param edition An optional integer vector indicating which CAPL edition to use for calculating pedometer scores (1, 2). Default is 2.
#' @return A dataframe with 2 variables: $valid representing an integer vector of dichotomous values (0-1) where 1 identifies a valid day of pedometry, and $score representing an integer vector of values representing pedometer scores based on the CAPL scoring system. 
#' @export
#' @examples 
#' # Use of the function with 1000 randomly generated step and wear time values
#' steps <- runif(1000, 1000, 30000)
#' wear_time <- runif(1000, 9.9, 12)
#' output <- capl_pedometer(steps, wear_time)
#' 
#' output$valid
#' output$score

capl_pedometer <- function(steps, wear_time, edition = 2) {
	edition <- as.integer(edition)
	loop <- function( steps, time_worn, edition ) {
		steps <- as.integer( steps )
		time_worn <- as.numeric( time_worn )
		step_score <- NA
		if( sum( is.na( c( steps, time_worn ) ), na.rm = TRUE ) > 0 ) {
			valid_day <- NA
			step_score <- NA
		} else {
			if( steps >= 1000 & steps <= 30000 & time_worn >= 10 ) {
				valid_day <- 1
				if(edition)
				if(edition == 1) {
					lookup <- data.frame( steps = c(15000, 12000, 9000, 6000), score = c(6, 5, 3, 1) )
				} else {
					lookup <- data.frame( steps = c(18000, 17000, 16000, 15000, 14000, 13500, 13000, 12500, 12000, 11500, 11000, 10500, 10000, 9500, 9000, 8500, 8000, 7500, 7000, 6500, 6000, 5000, 4000, 3000, 2000), score = c(25:1) )	
				}
				for(i in 1:nrow(lookup)) {
					if(steps >= lookup$steps[i]) {
						step_score <- lookup$score[i] 
						break
					}
				}
				if(is.na(step_score)) step_score <- 0
			} else {
				valid_day <- 0
				step_score <- NA
			} 
		}
		list( valid_day, step_score )
	}
	output <- as.vector(unlist(apply(X = data.frame(as.integer(steps), as.numeric(wear_time)), MARGIN = 1, FUN = function(x, y = edition) loop(x[1], x[2], edition))))
	output <- matrix(output, ncol = 2, byrow = TRUE)
	output <- data.frame(valid = output[,1], score = output[,2])
}