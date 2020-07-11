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

#' Compute a binary score for a response to a questionnaire item.
#'
#' @description
#' This function computes a binary score (0 = incorrect answer, 1 = correct answer) for a response to a questionnaire item based on the values set as 
#' answer(s) to the item.
#'
#' @export
#'
#' @param x an element or vector representing a response to a questionnaire item.
#' @param answer an element or vector representing the correct answer(s) to the questionnaire item. The answer argument does not have to match x in case 
#' for a correct answer to be computed.
#' 
#' @details
#' This function is called to compute scores for several knowledge and understanding questions in the CAPL-2 questionnaire and is also called by 
#' [get_fill_in_the_blanks_score()]. 
#'
#' @examples
#' get_binary_score(
#'   x = c(1:4, NA, ""),
#'	 answer = 3
#' )
#'
#' # [1]  0  0  1  0 NA  0
#' 
#' get_binary_score(
#'   x = c("20 minutes", "30 minutes", "60 minutes or 1 hour", "120 minutes or 2 hours"),
#'	 answer = "60 minutes or 1 hour"
#' )
#'
#' # [1] 0 0 1 0
#'
#' get_binary_score(
#'   x = c(1:5, "Heart", "hello, world", NA),
#'   answer = c(3, "heart")
#' )
#'
#' # [1]  0  0  1  0  0  1  0 NA
#'
#' @return returns 1 (if correct), 0 (if incorrect) or NA (if not valid).
get_binary_score <- function(x, answer) {
  return(
    unname(
      sapply(x, function(x) {
        if(sum(is.na(c(x, answer))) > 0) {
          NA
        } else if(tolower(x) %in% tolower(answer)) {
          1
        }else {
          0
        }
      })
    )
  )
}

#' Generate CAPL-2 demo (fake) raw data.
#'
#' @description
#' This function generates a data frame of CAPL-2 demo (fake) raw data containing the 60 required variables that the `capl` package needs to compute
#' scores and interpretations.
#'
#' @export
#'
#' @param n A numeric (integer) vector representing the number of rows of data to generate. By default, `n` is set to 500. 
#' 
#' @details
#' Other `capl` functions called by this function include: [validate_integer()].
#' [get_fill_in_the_blanks_score()]. 
#'
#' @examples
#' capl_demo_data <- get_capl_demo_data(10000)
#'
#' str(capl_demo_data)
#'
#' # 'data.frame':	10000 obs. of  60 variables:
#' #  $ age                            : int  9 10 8 8 11 9 12 NA 10 7 ...
#' #  $ gender                         : chr  "Girl" "Boy" "Boy" "Girl" ...
#' #  $ pacer_lap_distance             : num  20 15 20 20 15 15 15 20 15 20 ...
#' #  $ pacer_laps                     : int  5 112 150 46 51 82 43 189 55 91 ...
#' #  $ plank_time                     : int  238 66 95 173 299 172 169 33 277 152 ...
#' #  $ camsa_skill_score1             : int  9 3 7 NA 8 14 13 14 11 11 ...
#' #  $ camsa_time1                    : int  17 33 26 22 31 28 NA 24 12 11 ...
#' #  $ camsa_skill_score2             : int  12 11 12 9 NA 9 7 10 14 11 ...
#' #  $ camsa_time2                    : int  15 13 15 20 12 15 29 12 12 18 ...
#' #  $ steps1                         : int  29663 30231 3157 5751 23362 28283 ...
#' #  $ time_on1                       : chr  "05:00" "5:13am" "07:00" "8:00am" ...
#' #  $ time_off1                      : chr  "11:57pm" "10:57 pm" "10:57 pm" "11:57pm" ...
#' #  $ non_wear_time1                 : int  38 47 38 40 36 32 36 82 25 51 ...
#' #  $ steps2                         : int  29703 9142 5424 23763 3645 28625 3019 ...
#' #  $ time_on2                       : chr  "07:00" "07:48am" "6:07" "06:00" ...
#' #  $ time_off2                      : chr  "22:00" "21:00" "8:17pm" "10:57 pm" ...
#' #  $ non_wear_time2                 : int  5 34 41 60 84 18 19 47 66 55 ...
#' #  $ steps3                         : int  20380 10987 5885 13518 14385 30680 14120 ...
#' #  $ time_on3                       : chr  "07:00" "06:00" "6:07" "8:00am" ...
#' #  $ time_off3                      : chr  "11:13pm" "11:57pm" "21:00" "08:30pm" ...
#' #  $ non_wear_time3                 : int  54 70 16 36 72 16 89 86 26 81 ...
#' #  $ steps4                         : int  13224 20817 19640 2326 16605 25783 23078 ...
#' #  $ time_on4                       : chr  "07:48am" "5:13am" "5:13am" "6:07" ...
#' #  $ time_off4                      : chr  "11:13pm" NA "22:00" "23:00" ...
#' #  $ non_wear_time4                 : int  2 48 61 NA 81 81 2 30 35 14 ...
#' #  $ steps5                         : int  28408 8845 5802 6966 24499 18561 13771 ...
#' #  $ time_on5                       : chr  "5:13am" NA "06:00" "6:07" ...
#' #  $ time_off5                      : chr  "11:13pm" NA "11:57pm" "11:13pm" ...
#' #  $ non_wear_time5                 : int  75 10 70 45 77 75 90 61 17 72 ...
#' #  $ steps6                         : int  9581 18237 6377 3282 16898 15649 19890 ...
#' #  $ time_on6                       : chr  "6:13" "6:07" "07:00" "8:00am" ...
#' #  $ time_off6                      : chr  "11:57pm" "21:00" "10:57 pm" "8:17pm" ...
#' #  $ non_wear_time6                 : int  13 14 37 28 14 86 89 19 78 40 ...
#' #  $ steps7                         : int  8205 15351 16948 19442 4026 10830 4644 ...
#' #  $ time_on7                       : chr  "05:00" NA "07:48am" "6:07" ...
#' #  $ time_off7                      : chr  NA "22:00" "08:30pm" "08:30pm" ...
#' #  $ non_wear_time7                 : int  84 40 42 34 13 58 67 86 64 46 ...
#' #  $ self_report_pa                 : int  4 NA NA 7 1 1 6 7 6 6 ...
#' #  $ csappa1                        : int  2 1 1 1 2 1 4 3 3 3 ...
#' #  $ csappa2                        : int  3 3 1 4 4 2 3 1 4 4 ...
#' #  $ csappa3                        : int  1 2 4 1 2 4 1 4 4 1 ...
#' #  $ csappa4                        : int  4 1 3 4 2 3 1 2 2 4 ...
#' #  $ csappa5                        : int  2 4 2 2 4 1 1 1 3 1 ...
#' #  $ csappa6                        : int  2 2 2 3 4 3 2 3 1 1 ...
#' #  $ why_are_you_active1            : int  5 2 5 5 2 5 1 1 5 1 ...
#' #  $ why_are_you_active2            : int  4 5 2 4 3 1 5 1 4 1 ...
#' #  $ why_are_you_active3            : int  2 1 4 3 1 2 1 5 3 3 ...
#' #  $ feelings_about_pa1             : int  4 1 5 3 4 4 4 5 4 5 ...
#' #  $ feelings_about_pa2             : int  5 3 4 4 1 2 5 2 1 3 ...
#' #  $ feelings_about_pa3             : int  3 4 3 5 1 1 4 2 1 4 ...
#' #  $ pa_guideline                   : int  1 3 3 1 4 1 1 4 4 2 ...
#' #  $ cardiorespiratory_fitness_means: int  2 3 2 3 4 1 3 4 1 3 ...
#' #  $ muscular_strength_means        : int  1 1 4 2 4 4 2 1 1 3 ...
#' #  $ sports_skill                   : int  3 1 1 4 1 3 1 1 3 2 ...
#' #  $ pa_is                          : int  10 1 9 5 7 7 8 3 7 10 ...
#' #  $ pa_is_also                     : int  7 1 7 9 1 6 3 4 3 7 ...
#' #  $ improve                        : int  3 3 3 3 3 3 10 3 3 3 ...
#' #  $ increase                       : int  8 8 10 4 8 8 8 9 8 8 ...
#' #  $ when_cooling_down              : int  5 2 2 2 2 2 4 2 3 7 ...
#' #  $ heart_rate                     : int  4 9 7 4 4 4 4 4 5 7 ...
#'
#' @return Returns a data frame containing the 60 required variables that the `capl` package needs to compute scores and interpretations.
get_capl_demo_data <- function(n = 500) {
  try(
    if(is.na(n) | ! validate_integer(n) | n < 1) {
      stop("[CAPL error]: the n argument must be an integer >= 1.")
    } else {
      return(
        data.frame(
          "age" = sample(c(7:13, NA), n, replace = TRUE, prob = c(0.05, rep(0.17, 5), 0.05, 0.05)),
          "gender" = sample(c("Girl", "g", "Female", "f", "Boy", "b", "Male", "m", NA), n, replace = TRUE, prob = c(rep(0.11875, 8), 0.05)),
          "pacer_lap_distance" = sample(c(15, 20, NA), n, replace = TRUE, prob = c(0.475, 0.475, 0.05)),
          "pacer_laps" = sample(c(0:200, NA), n, replace = TRUE),
          "plank_time" = sample(c(0:300, NA), n, replace = TRUE),
          "camsa_skill_score1" = sample(c(0:14, NA), n, replace = TRUE, prob = c(rep(0.02, 4), rep(0.09222222, 11), 0.05)),
          "camsa_time1" = sample(c(10:35, NA), n, replace = TRUE, prob = c(rep(0.03653846, 26), 0.05)),
          "camsa_skill_score2" = sample(c(0:14, NA), n, replace = TRUE, prob = c(rep(0.02, 4), rep(0.09222222, 11), 0.05)),
          "camsa_time2" = sample(c(10:35, NA), n, replace = TRUE, prob = c(rep(0.03653846, 26), 0.05)),
          "steps1" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on1" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off1" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time1" = sample(c(0:90, NA), n, replace = TRUE),
          "steps2" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on2" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off2" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time2" = sample(c(0:90, NA), n, replace = TRUE),
          "steps3" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on3" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off3" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time3" = sample(c(0:90, NA), n, replace = TRUE),
          "steps4" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on4" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off4" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time4" = sample(c(0:90, NA), n, replace = TRUE),
          "steps5" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on5" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off5" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time5" = sample(c(0:90, NA), n, replace = TRUE),
          "steps6" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on6" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off6" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time6" = sample(c(0:90, NA), n, replace = TRUE),
          "steps7" = sample(c(900:31000, NA), n, replace = TRUE),
          "time_on7" = sample(c("5:13am", "05:00", "06:00", "07:00", "8:00am", "6:13", "07:48am", "6:07", NA), n, replace = TRUE),
          "time_off7" = sample(c("8:17pm", "21:00", "10:57 pm", "11:13pm", "22:00", "23:00", "08:30pm", "11:57pm", NA), n, replace = TRUE),
          "non_wear_time7" = sample(c(0:90, NA), n, replace = TRUE),
          "self_report_pa" = sample(c(1:7, NA), n, replace = TRUE, prob = c(rep(0.1, 5), rep(0.175, 2), 0.05)),
          "csappa1" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "csappa2" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "csappa3" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "csappa4" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "csappa5" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "csappa6" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "why_are_you_active1" = sample(c(1:5, NA), n, replace = TRUE, prob = c(rep(0.19, 5), 0.05)),
          "why_are_you_active2" = sample(c(1:5, NA), n, replace = TRUE, prob = c(rep(0.19, 5), 0.05)),
          "why_are_you_active3" = sample(c(1:5, NA), n, replace = TRUE, prob = c(rep(0.19, 5), 0.05)),
          "feelings_about_pa1" = sample(c(1:5, NA), n, replace = TRUE, prob = c(rep(0.19, 5), 0.05)),
          "feelings_about_pa2" = sample(c(1:5, NA), n, replace = TRUE, prob = c(rep(0.19, 5), 0.05)),
          "feelings_about_pa3" = sample(c(1:5, NA), n, replace = TRUE, prob = c(rep(0.19, 5), 0.05)),
          "pa_guideline" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "cardiorespiratory_fitness_means" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "muscular_strength_means" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "sports_skill" = sample(c(1:4, NA), n, replace = TRUE, prob = c(rep(4.210526, 4), 0.05)),
          "pa_is" = sample(c(1:10), n, replace = TRUE, prob = c(0.35, rep(0.06, 5), 0.35, rep(0.06, 3))),
          "pa_is_also" = sample(c(1:10), n, replace = TRUE, prob = c(0.35, rep(0.06, 5), 0.35, rep(0.06, 3))),
          "improve" = sample(c(1:10), n, replace = TRUE, prob = c(rep(0.06, 2), 0.7, rep(0.06, 7))),
          "increase" = sample(c(1:10), n, replace = TRUE, prob = c(rep(0.06, 7), 0.7, rep(0.06, 2))),
          "when_cooling_down" = sample(c(1:10), n, replace = TRUE, prob = c(0.06, 0.7, rep(0.06, 8))),
          "heart_rate" = sample(c(1:10), n, replace = TRUE, prob = c(rep(0.06, 3), 0.7, rep(0.06, 6)))
        )
      )
    }
  )
}

#' Add required CAPL-2 variables to a data frame of raw data if they are missing.
#'
#' @description
#' This function adds required CAPL-2 variables (see the Details section for a full list) to a data frame of raw data if they are missing. When missing
#' variables are added, the values for a given missing variable are set to NA. This function is called within [get_capl()] so that CAPL-2 score and
#' interpretation computations will run without errors in the presence of missing variables.
#'
#' @export
#'
#' @param raw_data a data frame of raw CAPL-2 data.
#'
#' @details
#' The required CAPL-2 variables include:
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
#' * pa_guideline
#' * cardiorespiratory_fitness_means
#' * muscular_strength_means
#' * sports_skill
#' * pa_is
#' * pa_is_also
#' * improve
#' * increase
#' * when_cooling_down
#' * heart_rate
#'
#' Examining the structure (see [str()]) of some CAPL-2 demo data (see [get_capl_demo_data()]) provides additional information about these variables.
#'
#' @examples
#' raw_data <- get_missing_capl_variables(raw_data)
#'
#' @return returns a merged data frame of raw data and missing required CAPL-2 variables (values are set to NA).
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
        paste0("feelings_about_pa", 1:3),
        "pa_guideline",
        "cardiorespiratory_fitness_means",
        "muscular_strength_means",
        "sports_skill",
        "pa_is",
        "pa_is_also",
        "improve",
        "increase",
        "when_cooling_down",
        "heart_rate"
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
#' @return returns a numeric (integer) vector with a value between 8 and 12 (if valid) or NA (if not valid).
validate_age <- function(x) {
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_number(x)
        if(is.na(x) | (x < 8 | x > 12)) {
          x <- NA
        } else {
          floor(x)
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

#' Check whether a CAPL domain score is numeric and within a valid range.
#'
#' @export
#'
#' @param x an element or vector representing a CAPL domain score.
#' @param domain a character element representing domains within CAPL (valid values are "pc", "db", "mc", "ku"; valid values are not
#' case-sensitive).
#'
#' @examples
#' validate_domain_score(
#'   x = c(34, 15, 10, 12.5, 25),
#'   domain = "pc"
#' )
#'
#' # [1]   NA 15.0 10.0 12.5 25.0
#'
#' @return returns a numeric element (if valid) or NA (if not valid).
validate_domain_score <- function(x = NA, domain = NA) {
  domain <- tolower(domain[1])
  return(
    unname(
      sapply(x, function(x) {
        x <- validate_number(x)
        if(is.na(x) | ! domain %in% c("pc", "db", "mc", "ku")) {
          return(NA)
        } else if(domain == "ku" & (x < 0 | x > 10)) {
          return(NA)
		} else if(domain == "db" & (x < 0 | x > 30 | is.na(validate_integer(x)))) {
          return(NA)
        }  else if(domain %in% c("pc", "mc") & (x < 0 | x > 30)) {
          return(NA)
        } else {
          return(x)
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
#' "breq", "pa_guideline", "cardiorespiratory_fitness_means", "muscular_strength_means", "sports_skill", "fill_in_the_blanks"; valid values are not
#' case-sensitive).
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
        if(is.na(x) | ! protocol %in% c("pc", "db", "csappa", "breq", "pa_guideline", "cardiorespiratory_fitness_means", "muscular_strength_means", "sports_skill", "fill_in_the_blanks")) {
          return(NA)
        } else if(protocol == "pc" & (x < 0 | x > 10)) {
          return(NA)
        } else if(protocol == "db" & ! validate_integer(x) | (x < 0 | x > 25)) {
          return(NA)
        } else if(protocol == "csappa" & (x < 1.8 | x > 7.5)) {
          return(NA)
        } else if(protocol == "breq" & (x < 1.5 | x > 7.5)) {
          return(NA)
        } else if(protocol %in% c("pa_guideline", "cardiorespiratory_fitness_means", "muscular_strength_means", "sports_skill") & (x < 0 | x > 1 | is.na(validate_integer(x)))) {
          return(NA)
        }  else if(protocol == "fill_in_the_blanks" & (x < 0 | x > 6 | is.na(validate_integer(x)))) {
          return(NA)
        } else {
          return(x)
        }
      })
    )
  )
}

#' Check whether a response to a given questionnaire item or scale is valid.
#'
#' @description
#' This function checks whether a vector for a given questionnaire item or scale is valid. This function calls
#' [validate_integer()] and [validate_number()] and is called by [get_adequacy_score()] and [get_predilection_score()]. This function can also be called
#' using `validate_scale()`.
#'
#' @export
#'
#' @param x a numeric (integer) element or vector representing the response to a questionnaire item (valid values are between the values set by the 
#' lower_bound and upper_bound argumetns).
#' @param lower_bound a numeric element representing the value below which x is invalid.
#' @param upper_bound a numeric element representing the value above which x is invalid.
#'
#' @examples
#' validate_scale(
#'   x = c(0:10, NA, "7"),
#'   lower_bound = 1,
#'   upper_bound = 7
#' )
#'
#' # [1] NA  1  2  3  4  5  6  7 NA NA NA NA  7
#'
#' @return Returns a numeric (integer) vector (if valid) or NA (if not valid).
validate_scale <- function(x, lower_bound = NA, upper_bound = NA) {
  x <- validate_integer(x)
  lower_bound <- validate_integer(lower_bound[1])
  upper_bound <- validate_integer(upper_bound[1])
  return(
    unname(
      sapply(x, function(x) {
        if(sum(is.na(c(x, lower_bound, upper_bound))) > 0 | x < lower_bound | x > upper_bound) {
          NA
        } else {
          x
        }
      })
    )
  )
}