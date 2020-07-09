#' Convert a 12-hour clock element to a 24-hour clock element.
#'
#' @export
#'
#' @param x a character element or vector in 12-hour clock format.
#'
#' @examples
#' get_24_hour_clock(c("5:00 am", "7:10PM", "21:37", NA, "", 9))
#'
#' # [1] "05:00" "19:10" "21:37" NA      NA      "9" 
#'
#' @return returns a 24-hour clock element (if valid) or the original element as a character element (if not valid).
get_24_hour_clock <- function(x = NA) {
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_character(x)
        if(grepl("am|pm", tolower(x))) {
          format(strptime(x, "%I:%M %p"), format = "%H:%M")
        } else {
          x
        }
      })
    )
  )
}

#' Add required CAPL variables to a data frame of raw data if they are missing.
#'
#' @description
#' This function adds required CAPL variables (see the Details section for a full list) to a data frame of raw data if they are missing. When missing
#' variables are added, the values for a given missing variable are set to NA. This function is called within [get_capl()] so that CAPL score and
#' interpretation computations will run without errors in the presence of missing variables.
#'
#' @export
#'
#' @param raw_data a data frame or tibble of raw CAPL data.
#'
#' @details
#' The required CAPL variables include:
#' * age
#' * gender
#' * pacer_lap_distance
#' * pacer_laps
#' * plank_time
#' * camsa_skill_score1
#' * camsa_time1
#' * camsa_skill_score2
#' * camsa_time2
#' * steps1
#' * time_on1
#' * time_off1
#' * non_wear_time1
#' * steps2
#' * time_on2
#' * time_off2
#' * non_wear_time2
#' * steps3
#' * time_on3
#' * time_off3
#' * non_wear_time3
#' * steps4
#' * time_on4
#' * time_off4
#' * non_wear_time4
#' * steps5
#' * time_on5
#' * time_off5
#' * non_wear_time5
#' * steps6
#' * time_on6
#' * time_off6
#' * non_wear_time6
#' * steps7
#' * time_on7
#' * time_off7
#' * non_wear_time7
#' * self_report_pa
#' * csappa1
#' * csappa2
#' * csappa3
#' * csappa4
#' * csappa5
#' * csappa6
#' * why_are_you_active1
#' * why_are_you_active2
#' * why_are_you_active3
#' * feelings_about_pa1
#' * feelings_about_pa2
#' * feelings_about_pa3
#'
#' @examples
#' raw_data <- get_missing_capl_variables(raw_data)
#'
#' @return returns a merged data frame of raw data and missing required CAPL variables (values are set to NA).
get_missing_capl_variables <- function(raw_data = NULL) {
  try(
    if(is.null(raw_data)) {
      stop("[CAPL error]: the raw_data argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(raw_data))) {
      stop("[CAPL error]: the raw_data argument must be a data frame or a tibble.")
    }
    else {
      required_variables <- c(
        "age",
        "gender",
        "pacer_lap_distance",
        "pacer_laps",
        "plank_time",
        "camsa_skill_score1",
        "camsa_time1",
        "camsa_skill_score2",
        "camsa_time2",
        "steps1",
        "time_on1",
        "time_off1",
        "non_wear_time1",
        "steps2",
        "time_on2",
        "time_off2",
        "non_wear_time2",
        "steps3",
        "time_on3",
        "time_off3",
        "non_wear_time3",
        "steps4",
        "time_on4",
        "time_off4",
        "non_wear_time4",
        "steps5",
        "time_on5",
        "time_off5",
        "non_wear_time5",
        "steps6",
        "time_on6",
        "time_off6",
        "non_wear_time6",
        "steps7",
        "time_on7",
        "time_off7",
        "non_wear_time7",
        "self_report_pa",
        paste0("csappa", 1:6),
        paste0("why_are_you_active", 1:3),
        paste0("feelings_about_pa", 1:3)
      )
      if(sum(required_variables %in% tolower(colnames(raw_data))) < length(required_variables)) {
        new_raw_data <- data.frame(sapply(required_variables, function(x) {
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

#' Check whether an element is numeric and between 8 and 12.
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_age(c(7:13, "", NA, "12"))
#'
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

#' Check whether an element is a character and not of length zero or "".
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_character(c("beginning", "progressing", "achieving", "excelling", "", NA, 7))
#'
#' # [1] "beginning"   "progressing" "achieving"   "excelling"   NA            NA 
#' # [7] "7"
#'
#' @return returns a character element (if valid) or NA (if not valid).
validate_character <- function(x) {
  return(
    unname(
      sapply(x, function(x) {
        x <- as.character(x)
        if(is.na(x) | length(x) == 0 | x == "") {
          x <- NA
        } else {
          x
        }
      })
    )
  )
}

#' Check whether an element can be classified as girl or boy.
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_gender(c("Girl", "GIRL", "g", "G", "Female", "f", "F", "", NA))
#'
#' # [1] "girl" "girl" "girl" "girl" "girl" "girl" "girl" NA     NA
#'
#' validate_gender(c("Boy", "BOY", "b", "B", "Male", "m", "M", "", NA))
#'
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

#' Check whether an element is an integer.
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_integer(c(2, 6, 3.3, "", NA, "6", "hello, world"))
#'
#' # [1]  2  6 NA NA NA  6 NA
#'
#' @return returns a numeric (integer) element (if valid) or NA (if not valid).
validate_integer <- function(x) {
  x <- validate_number(x)
  return(
    unname(
      sapply(x, function(x) {
        if(is.na(x) | x %% 1 > 0) {
          NA
        } else {
          x
        }
      })
    )
  )
}

#' Check whether an element is numeric.
#'
#' @export
#'
#' @param x an element or vector.
#'
#' @examples
#' validate_number(c(1:5, "5", "", NA, "hello, world!"))
#'
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
#' @param protocol a character element representing protocols within one of the four CAPL domains (valid values currently include "pc", "db", "csappa"
#' "breq"; valid values are not case-sensitive).
#'
#' @examples
#' validate_protocol_score(
#'   x = c(1, 5, 10, 11, -1, NA, "6"),
#'   protocol = "pc"
#' )
#'
#' # [1]  1  5 10 NA NA NA  6
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
validate_protocol_score <- function(x = NA, protocol = NA) {
  protocol <- tolower(protocol[1])
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_number(x)
        if(is.na(x) | ! protocol %in% c("pc", "db", "csappa", "breq")) {
          return(NA)
        } else if(protocol == "pc" & (x < 0 | x > 10)) {
          return(NA)
        } else if(protocol == "db" & (x < 0 | x > 25)) {
          return(NA)
        } else if(protocol == "csappa" & (x < 1.8 | x > 7.5)) {
          return(NA)
        } else if(protocol == "breq" & (x < 1.5 | x > 7.5)) {
          return(NA)
        } else {
          return(x)
        }
      })
    )
  )
}