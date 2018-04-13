#' Evaluates CAPL data against the IQR rule for outliers.
#'
#' This function evaluates CAPL data against the IQR rule for outliers based on age- and sex-specific distributions of the data. According to this rule, a value less than quartile 1 - (1.5 * the interquartile range) or greater than quartile 3 + (1.5 * the interquartile range) is considered an outlier. 
#' @param x A numeric vector.
#' @param age An integer vector.
#' @param sex A vector containing numeric or character values.
#' @param min_values An optional parameter that specifies the minimum number of values for a given age- and sex-specific distribution required before the function will evaluate the values within the x argument related to that distribution against the IQR rule for outliers. The default is 25. If a given age- and sex-specific distribution has less values than the minimum, NAs will be returned for all values related to that distribution.
#' @return An integer vector of dichotomous values (0-1) where 1 identifies a value as an outlier based on the IQR rule for outliers.
#' @export
#' @examples 
#' # Use of the function with 500 randomly generated values for 8- to 9-year-old males and females
#' random_values <- runif(500, 1, 100)
#' random_values[c(1:2, 499:500)] <- c(rep(-100, 2), rep(200, 2))
#' age <- c(rep(8, 250), rep(9, 250))
#' sex <- c(rep("male", 250), rep("female", 250))
#' capl_iqr(random_values, age, sex)

capl_iqr <- function(x, age, sex, min_values = 25) {
	min_values <- as.integer(min_values)
	if(is.na(min_values)) min_values <- 25
	loop <- function(x, age, sex, min_values, capl_df) {
		x <- as.numeric(x)
		age <- as.integer(age)
		if(sum(is.na(c(x, age, sex)), na.rm = TRUE) > 0) {
			output <- NA
		} else {
			subsetted_x <- subset(capl_df$x, capl_df$age == age & capl_df$sex == sex)
			if(length(subsetted_x) < min_values) {
				output <- NA
			} else {
				q <- stats::quantile(subsetted_x, na.rm = TRUE)
				q1 <- q[[2]]
				q3 <- q[[4]]
				iqr <- q3 - q1
				lower_bound <- q1 - (1.5 * iqr)
				upper_bound <- q3 + (1.5 * iqr)
				if(x < lower_bound | x > upper_bound) output <- 1 else output <- 0
			}
		}
	}
	capl_df <- data.frame(x = as.numeric(x), age = as.integer(age), sex)
	as.integer(apply(X = capl_df, MARGIN = 1, FUN = function(x, y = min_values, z = capl_df) loop(x[1], x[2], x[3], y, z)))
}