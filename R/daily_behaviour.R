#' Compute a daily behaviour domain score.
#'
#' @description
#' This function computes a daily behaviour domain score (`db_score`) based on the step and self-reported physical activity scores. This score
#' is used to compute the overall physical literacy score (`capl_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param step_score A numeric (integer) vector representing the pedometer steps score (valid values are integers between 0 and 25).
#' @param self_report_pa_score A numeric (integer) vector representing the self-reported physical activity score (valid values are
#' integers between 0 and 5).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' @examples
#' get_db_score(
#'   step_score = c(20, 6, 13, 5, NA, 4.5),
#'   self_report_pa_score = c(3, 2, 1, 4, 7, 3)
#' )
#'
#' # [1] 23  8 14  9 NA NA
#'
#' @return Returns a numeric (integer) vector with values between 0 and 30 (if valid) or NA (if not valid).
get_db_score <- function(step_score = NA, self_report_pa_score = NA) {
  try(
    if(var(c(length(step_score), length(self_report_pa_score))) == 0) {
      return(
        unname(
          apply(data.frame(step_score, self_report_pa_score), 1, function(x) {
            step_score <- validate_scale(x[1], 0, 25)
            self_report_pa_score <- validate_scale(x[2], 0, 5)
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
#' @description
#' This function computes pedometer wear time in decimal hours for a given day (e.g., `wear_time1`). This variable is used to compute the `step_average`
#' variable and the step score (`step_score`).
#'
#' @export
#'
#' @importFrom lubridate as.duration dminutes hm 
#' @importFrom stats var
#'
#' @param time_on A character vector representing the time (in 12- or 24-hour clock format) when the pedometer was first worn on a given day.
#' @param time_off A character vector representing the time (in 12- or 24-hour clock format) when the pedometer was removed at the end of a given
#' day.
#' @param non_wear_time A numeric vector representing the total time (in minutes) the pedometer was not worn during waking hours on a given day.
#'
#' @details
#' Other `capl` functions called by this function include: [get_24_hour_clock()] and [validate_number()].
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
#' @return Returns a numeric vector with values >= 0 (if valid) or NA (if not valid).
get_pedometer_wear_time <- function(time_on = NA, time_off = NA, non_wear_time = NA) {
  try(
    if(var(c(length(time_on), length(time_off), length(non_wear_time))) == 0) {
      return(
        unname(
          apply(data.frame(time_on, time_off, non_wear_time), 1, function(x) {
            time_on <- suppressWarnings(hm(get_24_hour_clock(x[1]), quiet = TRUE))
            time_off <- suppressWarnings(hm(get_24_hour_clock(x[2]), quiet = TRUE))
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
#' This function computes a score (`self_report_pa_score`) for a response to "During the past week (7 days), on how many days were you physically active for
#' a total of at least 60 minutes per day? (all the time you spent in activities that increased your heart rate and made you breathe hard)?" in the CAPL-2
#' Questionnaire. This score is used to compute the daily behaviour domain score (`db_score`).
#'
#' @export
#'
#' @param x A numeric (integer) vector representing the self-reported physical activity question (valid values are integers between 0 and 7).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' @examples
#' get_self_report_pa_score(c(1, 8, 3, 4, 5, 2, 7))
#'
#' # [1]  0 NA  2  3  4  1  5
#'
#' @return Returns a numeric (integer) vector with values between 0 and 5 (if valid) or NA (if not valid).
get_self_report_pa_score <- function(x = NA) {
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_scale(x, 0, 7)
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

#' Compute average daily steps taken.
#'
#' @description
#' This function computes the daily arithmetic mean of a week of steps taken as measured by a pedometer (`step_average`). This variable is used to compute
#' the step score (`step_score`). 
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param raw_data A data frame that includes seven days of pedometer steps and their corresponding on and off times. See Details for how these variables
#' must be named.
#' 
#' @details
#' This function will throw an error unless the following variables are found in the `raw_data` argument:
#' * `steps1`
#' * `steps2`
#' * `steps3`
#' * `steps4`
#' * `steps5`
#' * `steps6`
#' * `steps7` 
#' * `time_on1`
#' * `time_on2`
#' * `time_on3`
#' * `time_on4`
#' * `time_on5`
#' * `time_on6`
#' * `time_on7`
#' * `time_off1`
#' * `time_off2`
#' * `time_off3`
#' * `time_off4`
#' * `time_off5`
#' * `time_off6`
#' * `time_off7`
#'
#' There must be at least three valid days for an arithmetic mean to be computed. If only three valid days, one of the step values from a valid 
#' day will be randomly sampled and used for the fourth valid day before computing the mean. 
#'
#' Other `capl` functions called by this function include: [validate_steps()] and [get_pedometer_wear_time()].
#'
#' @examples
#' capl_demo_data <- get_capl_demo_data(10)
#' 
#' get_step_average(capl_demo_data)$step_average
#' 
#' # [1] 18365 12655 15493 12966 11396 13954 18456 13589 17543 11276
#'
#' @return Returns a data frame with nine columns: `steps1` (validated), `steps2` (validated), `steps3` (validated), `steps4` (validated), `steps5` (validated), `steps6` (validated), `steps7` (validated), `valid_days` and `step_average`. The steps are validated with the [validate_steps()] function.
get_step_average <- function(raw_data = NULL) {
  try(
    if(is.null(raw_data)) {
      stop("[CAPL error]: the raw_data argument is missing.")
    } else if(! isTRUE("data.frame" %in% class(raw_data))) {
      stop("[CAPL error]: the raw_data argument must be a data frame.")
    } else {
      required_variables <- c(paste0("steps", 1:7), paste0("time_on", 1:7), paste0("time_off", 1:7), paste0("non_wear_time", 1:7))
      if(sum(required_variables %in% colnames(raw_data)) != length(required_variables)) {
        stop("[CAPL error]: the raw_data argument does not include all required variables.")
      } else {
        steps_df <- data.frame(rows = 1:nrow(raw_data))
        for(i in 1:7) {
          steps_df[paste0("day", i)] <- validate_steps(as.vector(raw_data[[paste0("steps", i)]]), get_pedometer_wear_time(raw_data[paste0("time_on", i)], raw_data[paste0("time_off", i)], raw_data[paste0("non_wear_time", i)]))
        }
        steps_df <- steps_df[,-1]
        steps_df$valid_days <- apply(steps_df, 1, function(x) sum(! is.na(x)))
        
        steps_df$step_average <- apply(steps_df, 1, function(x) {
          valid_days <- x[8]
          if(is.na(valid_days) | valid_days < 3) {
            NA
          } else if(valid_days == 3) {
            round(mean(c(x[-8], sample(x[-8][which(! is.na(x[-8]))], 1)), na.rm = TRUE))
          } else if(valid_days >= 4) {
            round(mean(x[-8], na.rm = TRUE))
          } else {
            NA
          }
        })
        return(steps_df)
      }
    }
  )
}

#' Compute a step score.
#'
#' @description
#' This function computes a step score (`step_score`) based on the average daily steps taken as measured by a pedometer. This score is used to compute the 
#' daily behaviour domain score (`db_score`).
#'
#' @export
#'
#' @param step_average A numeric vector representing average daily steps taken. See [get_step_average()].
#'
#' @details
#' Other `capl` functions called by this function include: [validate_number()].
#'
#' @examples
#' capl_demo_data <- get_capl_demo_data(10)
#'
#' step_average <- get_step_average(capl_demo_data)$step_average
#'
#' get_step_score(step_average)
#'
#' # [1] 25 18 22 18 15 20 25 20 24 15
#'
#' @return Returns a numeric (integer) vector with values between 0 and 25 (if valid) or NA (if not valid).
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

#' Check whether daily steps as measured by a pedometer are valid.
#'
#' @description
#' This function checks whether daily steps as measured by a pedometer are valid. The variables from this function are used to compute `step_average` and
#' the step score (`step_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param steps A numeric (integer) vector representing the steps taken on a given day (valid values are between 1000 and 30000).
#' @param wear_time A numeric vector representing the duration of time (in decimal hours) that a pedometer was worn on a given day (valid values are >= 10.0
#' hours).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()] and [validate_number()].
#'
#' @examples
#' validate_steps(
#'   steps = c(5400, 11001, 999, 31000, 8796),
#'   wear_time = c(10.1, 12.6, 10.2, 10.9, 9.5)
#' )
#'
#' # [1] "yes" "yes" "no"  "no"  "no" 
#'
#' @return Returns the `steps` argument (if valid) or NA (if not valid).
validate_steps <- function(steps = NA, wear_time = NA) {
  try(
    if(var(c(length(steps), length(wear_time))) == 0) {
      return(
        unname(
          apply(data.frame(steps, wear_time), 1, function(x) {
            steps <- validate_scale(x[1], 1000, 30000)
            wear_time <- validate_number(x[2])
            if(sum(is.na(c(steps, wear_time))) > 0 | wear_time < 10) {
              NA
            } else {
              steps
            }
          })
        )
      )
    } else {
      stop(stop("[CAPL error]: the steps and wear_time arguments must be the same length."))
    }
  )
}