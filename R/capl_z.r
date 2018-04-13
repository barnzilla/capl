#' Converts CAPL data to age- and sex-specific z-scores.
#'
#' This function converts CAPL data to z-scores based on age- and sex-specific distributions of the vector.
#' @param x A numeric vector.
#' @param age An integer vector.
#' @param sex A vector containing numeric or character values.
#' @return A vector of age- and sex-specific z-scores.
#' @export
#' @examples 
#' # Use of the function with 500 randomly generated values for 8- to 9-year-old males and females
#' random_values <- runif(500, 1, 100)
#' random_values[c(1:2, 499:500)] <- c(rep(-100, 2), rep(200, 2))
#' age <- c(rep(8, 250), rep(9, 250))
#' sex <- c(rep("male", 250), rep("female", 250))
#' capl_z(random_values, age, sex)

capl_z <- function(x, age, sex) {
	loop <- function(x, age, sex, capl_df) {
		x <- as.numeric(x)
		age <- as.integer(age)
		if(sum(is.na(c(x, age, sex)), na.rm = TRUE) > 0) {
			output <- NA
		} else {
			subsetted_x <- subset(capl_df$x, capl_df$age == age & capl_df$sex == sex)
			output <- (x - mean(subsetted_x, na.rm = TRUE)) / stats::sd(subsetted_x, na.rm = TRUE)
		}
	}
	capl_df <- data.frame(x = as.numeric(x), age = as.integer(age), sex)
	as.numeric(apply(X = capl_df, MARGIN = 1, FUN = function(x, y = capl_df) loop(x[1], x[2], x[3], y)))
}