#' Check whether an element is numeric and between 8 and 12
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_age(c(7:13, "", NA, "12"))
#' # [1] NA  8  9 10 11 12 NA NA NA 12
#'
#' @return returns a numeric element with a value between 8 and 12 (if valid) or NA (if not valid).
validate_age <- function(x) {
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_number(x)
        if(is.na(x) | (x < 8 | x > 12)) {
          x <- NA
        } else {
          x
        }
       })
    )
  )
}

#' Check whether an element can be classified as girl or boy
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_gender(c("Girl", "GIRL", "g", "G", "Female", "f", "F", "", NA))
#' # [1] "girl" "girl" "girl" "girl" "girl" "girl" "girl" NA     NA
#'
#' validate_gender(c("Boy", "BOY", "b", "B", "Male", "m", "M", "", NA))
#' # [1] "boy" "boy" "boy" "boy" "boy" "boy" "boy" NA    NA
#'
#' @return returns a character element with a value of "girl" or "boy" (if valid) or NA (if not valid).
validate_gender <- function(x) {
  return(
    unname(
      sapply(x, function(x) {
        x <- tolower(as.character(x))
        if(is.na(x)) {
          x <- NA
        } else if(x %in% c("girl", "female", "f", "g")) {
          x <- "girl"
        } else if(x %in% c("boy", "male", "m", "b")) {
          x <- "boy"
        } else {
          x <- NA
        }
      })
    )
  )
}

#' Check whether an element is numeric
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_number(c(1:5, "5", "", NA, "hello, world!"))
#' # [1]  1  2  3  4  5  5 NA NA NA
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
validate_number <- function(x) {
  return(unname(sapply(x, function(x) suppressWarnings(as.numeric(x)))))
}

#' Check whether a physical competence score is numeric and between 0 and 10
#'
#' @export
#'
#' @param x an element or vector representing a physical competence protocol score.
#'
#' @examples
#' validate_pc_protocol_score(c(1, 5, 10, 11, -1, NA, "6"))
#' # [1]  1  5 10 NA NA NA  6
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
validate_pc_protocol_score <- function(x) {
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_number(x)
        if(is.na(x) | x < 0 | x > 10) {
          return(NA)
        } else {
          return(x)
        }
      })
    )
  )
}