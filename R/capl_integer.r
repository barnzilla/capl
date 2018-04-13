#' Converts CAPL interpretations from strings to integers.
#'
#' This function converts CAPL interpretations from strings to integers.
#' @param x A character vector (beginning, progressing, achieving, excelling).
#' @return An integer vector (1 = beginning, 2 = progressing, 3 = achieving, 4 = excelling).
#' @export
#' @examples 
#' capl_interpretations <- c("beginning", "progressing", "achieving", "excelling")
#' capl_integer(capl_interpretations)

capl_integer <- function(x) {
	x <- tolower(as.character(x))
	lookup <- c(beginning = 1, progressing = 2, achieving = 3, excelling = 4)
	output <- as.integer(lookup[x])
	output
}