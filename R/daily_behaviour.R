#' Compute a daily behaviour domain score based on the step and self-reported physical activity scores. 
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param step_score a numeric (integer) element or vector representing the pedometer steps protocol score (valid values are integers between 0 and 25).
#' @param self_report_pa_score a numeric (integer) element or vector representing the self-reported physical activity protocol score (valid values are
#' integers between 0 and 5).
#'
#' @examples
#' get_db_score(
#'   step_score = c(20, 6, 13, 5, NA, 4.5),
#'   self_report_pa_score = c(3, 2, 1, 4, 7, 3)
#' )
#'
#' # [1] 23  8 14  9 NA NA
#'
#' @return returns a numeric (integer) element (if valid) or NA (if not valid).
get_db_score <- function(step_score = NA, self_report_pa_score = NA) {
  try(
    if(var(c(length(step_score), length(self_report_pa_score))) == 0) {
      return(
        unname(
          apply(data.frame(step_score, self_report_pa_score), 1, function(x) {
            step_score <- validate_integer(validate_protocol_score(x[1], "db"))
            self_report_pa_score <- validate_integer(validate_protocol_score(x[2] * 5, "db"))
            if(! is.na(self_report_pa_score)) {
              self_report_pa_score <- self_report_pa_score / 5
            }
            if(sum(is.na(c(step_score, self_report_pa_score))) > 0) {
              return(NA)
            } else {
              return(sum(step_score, self_report_pa_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the step_score and self_report_pa_score arguments must be the same length.")
    }
  )
}

#' Compute pedometer wear time in decimal hours for a given day.
#'
#' @export
#'
#' @importFrom lubridate as.duration dminutes hm 
#' @importFrom stats var
#'
#' @param time_on a character element or vector representing the time (in 12- or 24-hour clock format) when the pedometer was first worn on a given day.
#' @param time_off a character element or vector representing the time (in 12- or 24-hour clock format) when the pedometer was removed at the end of a given
#' day.
#' @param non_wear_time a numeric element or vector representing the total time (in minutes) the pedometer was not worn during waking hours on a given day.
#'
#' @examples
#' get_pedometer_wear_time(
#'   time_on = c("6:23", "5:50 am", NA),
#'   time_off = c("21:37", "9:17pm", ""),
#'   c(60, 90, 0)
#' )
#'
#' # [1] 14.23 13.95    NA
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
get_pedometer_wear_time <- function(time_on = NA, time_off = NA, non_wear_time = NA) {
  try(
    if(var(c(length(time_on), length(time_off), length(non_wear_time))) == 0) {
      return(
        unname(
          apply(data.frame(time_on, time_off, non_wear_time), 1, function(x) {
            time_on <- suppressWarnings(hm(get_24_hour_clock(x[1])))
            time_off <- suppressWarnings(hm(get_24_hour_clock(x[2])))
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
    } else {
      stop("[CAPL error]: the time_on, time_off and non_wear_time arguments must be the same length.")
    }
  )
}

#' Compute a score for a response to the self-reported physical activity question.
#'
#' @description
#' This function computes a score for a response to "During the past week (7 days), on how many days were you physically active for a total of at least 60
#' minutes per day? (all the time you spent in activities that increased your heart rate and made you breathe hard)?". This score is used to compute the 
#' daily behaviour domain score.
#'
#' @export
#'
#' @param x a numeric (integer) element or vector representing the self-reported physical activity question (valid values are integers between 0 and 7).
#'
#' @examples
#' get_self_report_pa_score(c(1, 8, 3, 4, 5, 2, 7))
#'
#' # [1]  0 NA  2  3  4  1  5
#'
#' @return returns a numeric (integer) element (if valid) or NA (if not valid).
get_self_report_pa_score <- function(x = NA) {
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_integer(x)
        if(is.na(x)) {
          return(NA)
        } else if(x > 0 & x <= 1) {
          return(0)
        } else if(x == 2) {
          return(1)
        } else if(x == 3) {
          return(2)
        } else if(x == 4) {
          return(3)
        } else if(x == 5) {
          return(4)
        } else if(x > 5 & x <= 7) {
          return(5)
        } else {
          return(NA)
        }
      })
    )
  )
}

#' Compute the daily arithmetic mean of a week of steps taken as measured by a pedometer. 
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param steps a data frame or tibble with seven numeric columns where each column represents steps taken on a given day. 
#' @param valid_steps a data frame or tibble with seven character columns with "yes" or "no" elements representing whether the steps taken on a given day
#' are valid. See [validate_steps()].
#' 
#' @details
#' There must be at least three valid days for an arithmetic mean to be computed. If only three valid days, one of the step elements from a valid 
#' day will be randomly sampled and used for the fourth valid day before computing the mean. The [set.seed()] function is set to 93 so that the results can
#' be duplicated if necessary.
#'
#' @examples
#' set.seed(93)
#' 
#' steps <- data.frame(
#'   steps1 = sample(900:31000, 10, replace = TRUE),
#'   steps2 = sample(900:31000, 10, replace = TRUE),
#'   steps3 = sample(900:31000, 10, replace = TRUE),
#'   steps4 = sample(900:31000, 10, replace = TRUE),
#'   steps5 = sample(900:31000, 10, replace = TRUE),
#'   steps6 = sample(900:31000, 10, replace = TRUE),
#'   steps7 = sample(900:31000, 10, replace = TRUE)
#' )
#'
#' valid_steps <- data.frame(
#'   valid_steps1 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes"),
#'   valid_steps2 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.15, 0.85)) == 0, "no", "yes"),
#'   valid_steps3 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes"),
#'   valid_steps4 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.4, 0.6)) == 0, "no", "yes"),
#'   valid_steps5 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes"),
#'   valid_steps6 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.5, 0.5)) == 0, "no", "yes"),
#'   valid_steps7 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes")
#' )
#' 
#' get_step_average(steps, valid_steps)
#' 
#' # [1] 15833 13599 21178 22654 17994 17194 24472  8791  9058 20756
#'
#' @return returns a rounded numeric element (if valid) or NA (if not valid).
get_step_average <- function(steps = NULL, valid_steps = NULL) {
  try(
    if(is.null(steps)) {
      stop("[CAPL error]: the steps argument is missing.")
    } else if(is.null(valid_steps)) {
      stop("[CAPL error]: the valid_steps argument is missing.")
    } else if(! isTRUE("data.frame" %in% class(steps))) {
      stop("[CAPL error]: the steps argument must be a data frame or a tibble.")
    } else if(! isTRUE("data.frame" %in% class(valid_steps))) {
      stop("[CAPL error]: the valid_steps argument must be a data frame or a tibble.")
    } else if(ncol(steps) != 7) {
      stop("[CAPL error]: the steps argument must have 7 columns (steps1, steps2, steps3, steps4, steps5, steps6, steps7.")
    } else if(ncol(valid_steps) != 7) {
      stop("[CAPL error]: the valid_steps argument must have 7 columns (valid_steps1, valid_steps2, valid_steps3, valid_steps4, valid_steps5, valid_steps6, valid_steps7.")
    } else if(var(c(nrow(steps), nrow(valid_steps))) != 0) {
      stop("[CAPL error]: the steps and valid_steps arguments must be the same length.")
    } else {
      steps <- apply(steps, 2, validate_number)
      valid_steps <- apply(valid_steps, 2, validate_character)
      valid_days <- apply(valid_steps, 1, function(x) sum(x == "yes", na.rm = TRUE))
      for(i in 1:7) {
        steps[, i] <- ifelse(is.na(valid_steps[, i]) | valid_steps[, i] == "no", NA, steps[, i])
      }
      x <- cbind(steps, valid_days)
      number_of_columns <- ncol(x)
      return(
        unname(
          apply(x, 1, function(x) {
            valid_days <- x[number_of_columns]
            missing_steps <- sum(is.na(x[-number_of_columns]))
            if(is.na(valid_days) | missing_steps > 4 | valid_days < 3) {
              NA
            } else if(valid_days == 3) {
			  set.seed(93)
              round(mean(c(x[-number_of_columns], sample(x[-number_of_columns][which(! is.na(x[-number_of_columns]))], 1)), na.rm = TRUE))
            } else if(valid_days >= 4) {
              round(mean(x[-number_of_columns], na.rm = TRUE))
            } else {
              NA
            }
          })
        )
      )
    }
  )
}

#' Compute a step score based on the average daily steps taken as measured by a pedometer.
#'
#' @export
#'
#' @param step_average a numeric element or vector representing average daily steps taken. See [get_step_average()].
#'
#' @examples
#' set.seed(93)
#' 
#' steps <- data.frame(
#'   steps1 = sample(900:31000, 10, replace = TRUE),
#'   steps2 = sample(900:31000, 10, replace = TRUE),
#'   steps3 = sample(900:31000, 10, replace = TRUE),
#'   steps4 = sample(900:31000, 10, replace = TRUE),
#'   steps5 = sample(900:31000, 10, replace = TRUE),
#'   steps6 = sample(900:31000, 10, replace = TRUE),
#'   steps7 = sample(900:31000, 10, replace = TRUE)
#' )
#'
#' valid_steps <- data.frame(
#'   valid_steps1 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes"),
#'   valid_steps2 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.15, 0.85)) == 0, "no", "yes"),
#'   valid_steps3 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes"),
#'   valid_steps4 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.4, 0.6)) == 0, "no", "yes"),
#'   valid_steps5 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes"),
#'   valid_steps6 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.5, 0.5)) == 0, "no", "yes"),
#'   valid_steps7 = ifelse(sample(0:1, 10, replace = TRUE, prob = c(0.3, 0.7)) == 0, "no", "yes")
#' )
#' 
#' step_average <- get_step_average(steps, valid_steps)
#'
#' get_step_score(step_average)
#'
#' # [1] 22 20 25 25 24 24 25 10 11 25
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
get_step_score <- function(step_average = NA) {
  return(
    unname(
      sapply(step_average, function(x) {
        step_average <- validate_number(x)
        if(is.na(step_average)) {
          return(NA)
        } else if(step_average >= 18000) {
          return(25)
        } else if(step_average >= 17000) {
          return(24)
        } else if(step_average >= 16000) {
          return(23)
        } else if(step_average >= 15000) {
          return(22)
        } else if(step_average >= 14000) {
          return(21)
        } else if(step_average >= 13500) {
          return(20)
        } else if(step_average >= 13000) {
          return(19)
        } else if(step_average >= 12500) {
          return(18)
        } else if(step_average >= 12000) {
          return(17)
        } else if(step_average >= 11500) {
          return(16)
        } else if(step_average >= 11000) {
          return(15)
        } else if(step_average >= 10500) {
          return(14)
        } else if(step_average >= 10000) {
          return(13)
        } else if(step_average >= 9500) {
          return(12)
        } else if(step_average >= 9000) {
          return(11)
        } else if(step_average >= 8500) {
          return(10)
        } else if(step_average >= 8000) {
          return(9)
        } else if(step_average >= 7500) {
          return(8)
        } else if(step_average >= 7000) {
          return(7)
        } else if(step_average >= 6500) {
          return(6)
        } else if(step_average >= 6000) {
          return(5)
        } else if(step_average >= 5000) {
          return(4)
        } else if(step_average >= 4000) {
          return(3)
        } else if(step_average >= 3000) {
          return(2)
        } else if(step_average >= 2000) {
          return(1)
        } else if(step_average >= 1000) {
          return(0)
        } else {
          return(NA)
        }
      })
    )
  )
}

#' Sum the number of valid days that a pedometer was worn.
#'
#' @export
#'
#' @param x a data frame or tibble with seven columns where each column represents a day of the week when a pedometer was worn and contains "yes" or 
#' "no" values. See [validate_steps()].
#'
#' @examples
#' x <- data.frame(
#'   valid_steps1 = c("yes", "no", "yes"),
#'   valid_steps2 = c("no", "no", "yes"),
#'   valid_steps3 = c("yes", "yes", "yes"),
#'   valid_steps4 = c(NA, "no", "yes"),
#'   valid_steps5 = c("yes", "", "yes"),
#'   valid_steps6 = c("no", "yes", "yes"),
#'   valid_steps7 = c("yes", "yes", "no")
#' )
#'
#' get_valid_days(x)
#'
#' # [1] 4 3 6
#'
#' @return returns a numeric element (if valid) or 0 (if not valid).
get_valid_days <- function(x = NULL) {
  try(
    if(is.null(x)) {
      stop("[CAPL error]: the x argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(x))) {
      stop("[CAPL error]: the x argument must be a data frame or a tibble.")
    } else {
      x <- apply(x, 2, validate_character)
      apply(x, 1, function(x) sum(x == "yes", na.rm = TRUE))
    }
  )
}

#' Check whether a response to the self-reported physical activity question is valid.
#'
#' @description
#' This function checks whether the response to "During the past week (7 days), on how many days were you physically active for a total of at least 60
#' minutes per day? (all the time you spent in activities that increased your heart rate and made you breathe hard)?" is valid. A valid response is an integer
#' between 0 and 7. Data should be processed using this function before calling [get_self_report_pa_score()].
#'
#' @export
#'
#' @param x a numeric (integer) element or vector representing the steps taken on a given day (valid values are between 0 and 7).
#'
#' @examples
#' validate_self_report_pa(c(1:8, 5.5, NA, "", "5"))
#'
#' # [1]  1  2  3  4  5  6  7 NA NA NA NA  5
#'
#' @return returns a numeric (integer) element (if valid) or NA (if not valid).
validate_self_report_pa <- function(x) {
  x <- validate_integer(x)
  return(
    unname(
      sapply(x, function(x) {
        if(is.na(x) | x < 0 | x > 7) {
          NA
        } else {
          x
        }
      })
    )
  )
}

#' Check whether daily steps as measured by a pedometer are valid.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param steps a numeric (integer) element or vector representing the steps taken on a given day (valid values are between 1000 and 30000).
#' @param wear_time a numeric element representing the duration of time (in decimal hours) that a pedometer was worn on a given day (valid values are >= 10.0
#' hours).
#'
#' @examples
#' validate_steps(
#'   steps = c(5400, 11001, 999, 31000, 8796),
#'   wear_time = c(10.1, 12.6, 10.2, 10.9, 9.5)
#' )
#'
#' # [1] "yes" "yes" "no"  "no"  "no" 
#'
#' @return returns "yes" (if valid) or "no" (if not valid).
validate_steps <- function(steps = NA, wear_time = NA) {
  try(
    if(var(c(length(steps), length(wear_time))) == 0) {
      return(
        unname(
          apply(data.frame(steps, wear_time), 1, function(x) {
            steps <- validate_number(x[1])
            wear_time <- validate_number(x[2])
            if(sum(is.na(c(steps, wear_time))) > 0 | steps %% 1 > 0) {
              "no"
            } else if(steps < 1000 | steps > 30000 | wear_time < 10) {
              "no"
            } else {
              "yes"
            }
          })
        )
      )
    } else {
      stop(stop("[CAPL error]: the steps and wear_time arguments must be the same length."))
    }
  )
}