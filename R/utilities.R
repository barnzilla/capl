#' Add mandatory CAPL variables to a data frame of raw data if they are missing.
#'
#' @export
#'
#' @param raw_data a data frame or tibble of raw CAPL data.
#'
#' @examples
#' raw_data <- add_missing_capl_variables(raw_data)
#'
#' @return returns a merged data frame of raw data and missing mandatory CAPL variables (values are set to NA).
add_missing_capl_variables <- function(raw_data = NULL) {
  try(
    if(is.null(raw_data)) {
      stop("[CAPL error]: the raw_data argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(raw_data))) {
      stop("[CAPL error]: the raw_data argument must be a data frame or a tibble.")
    }
    else {
      mandatory_variables <- c(
        "age",
        "gender",
        "pacer_lap_distance",
        "pacer_laps",
        "plank_time",
        "camsa_skill_score1",
        "camsa_time1",
        "camsa_skill_score2",
        "camsa_time2"
      )
      if(sum(mandatory_variables %in% tolower(colnames(raw_data))) < length(mandatory_variables)) {
        new_raw_data <- data.frame(sapply(mandatory_variables, function(x) {
          if(! x %in% tolower(colnames(raw_data))) {
            rep(NA, nrow(raw_data))
          } else {
            raw_data[x]
          }
        }))
        return(
          cbind(raw_data, new_raw_data[! colnames(new_raw_data) %in% colnames(raw_data)])
        )
      } else {
        return(raw_data)
      }
    }
  )
}

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

#' Check whether a CAPL protocol score is numeric and within a valid range.
#'
#' @export
#'
#' @param x an element or vector representing a CAPL protocol score.
#' @param protocol a character element representing protocols within one of the four CAPL domains (valid values currently include "pc"; valid values are 
#' not case-sensitive).
#'
#' @examples
#' validate_protocol_score(
#'   x = c(1, 5, 10, 11, -1, NA, "6"),
#'   protocol = "pc"
#' )
#' # [1]  1  5 10 NA NA NA  6
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
validate_protocol_score <- function(x = NA, protocol = NA) {
  protocol <- tolower(protocol[1])
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_number(x)
        if(is.na(x) | ! protocol %in% c("pc")) {
          return(NA)
        } else if(protocol == "pc" & (x < 0 | x > 10)) {
          return(NA)
        } else {
          return(x)
        }
      })
    )
  )
}