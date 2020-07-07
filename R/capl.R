#' The main (wrapper) function in the CAPL package that computes CAPL scores and interpretations from raw data.
#'
#' @export
#'
#' @param raw_data a data frame or tibble of raw CAPL data.
#' @param sort a character element representing how the variables in the returned data frame should be sorted (valid values are "abc" and "zyx"; valid
#' values are not case-sensitive). This argument is set to "abc" by default.
#'
#' @examples
#' get_capl(raw_data)
#'
#' @return returns a merged data frame of raw data and CAPL scores and interpretations.
get_capl <- function(raw_data = NULL, sort = "abc") {
  try(
    if(is.null(raw_data)) {
      stop("[CAPL error]: the raw_data argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(raw_data))) {
      stop("[CAPL error]: the raw_data argument must be a data frame or a tibble.")
    }
    else {
      sort <- tolower(sort[1])
      raw_data <- add_missing_capl_variables(raw_data)
      raw_data$pacer_laps_20m <- get_pacer_20m_laps(raw_data$pacer_lap_distance, raw_data$pacer_laps)
      raw_data$pacer_score <- get_pacer_score(raw_data$pacer_laps_20m)
      raw_data$pacer_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$pacer_laps_20m, "pacer")
      raw_data$plank_score <- get_plank_score(raw_data$plank_time)
      raw_data$plank_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$plank_time, "plank")
      raw_data$camsa_time_score1 <- get_camsa_time_score(raw_data$camsa_time1)
      raw_data$camsa_time_score2 <- get_camsa_time_score(raw_data$camsa_time2)
      raw_data$camsa_score1 <- get_camsa_trial_score(raw_data$camsa_skill_score1, raw_data$camsa_time_score1)
      raw_data$camsa_score2 <- get_camsa_trial_score(raw_data$camsa_skill_score2, raw_data$camsa_time_score2)
      raw_data$camsa_overall_score <- get_camsa_overall_score(raw_data$camsa_score1, raw_data$camsa_score2)
      raw_data$camsa_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$camsa_overall_score, "camsa")
      raw_data$pc_score <- get_pc_score(raw_data$pacer_score, raw_data$plank_score, raw_data$camsa_overall_score)
      raw_data$pc_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$pc_score, "pc")
      raw_data$pc_status <- get_capl_domain_status(raw_data[c("pc_score", "pc_interpretation", "pacer_score", "plank_score", "camsa_overall_score")])
      if(is.na(sort) | is.null(sort) | sort == "" | length(sort) == 0) {
        # Don't reorder variables in raw_data
      } else if(sort == "abc") {
        raw_data <- raw_data[order(colnames(raw_data), decreasing = FALSE)]
      }  else if(sort == "zyx") {
        raw_data <- raw_data[order(colnames(raw_data), decreasing = TRUE)]
      } else {
        # Don't reorder variables in raw_data
      }
      return(raw_data)
    }
  )
}

#' Compute the status of a CAPL domain.
#'
#' @export
#'
#' @param x a data frame or tibble. The first column must be CAPL domain scores, the second column must be CAPL domain interpretations and 
#' the remaining columns must be protocol scores for the domain.
#'
#' @examples
#' x <- data.frame(
#'   domain_score = c(5, 10, 15, 20, 25, "", NA),
#'     interpretation = c("beginning", "progressing", NA, "achieving", "excelling", NA, NA),
#'     protocol_score1 = c(NA, 2, 3, 7, 10, NA, NA),
#'     protocol_score2 = c(3, 5, 7, 12, 10, NA, NA),
#'     protocol_score3 = c(2, 3, 10, 6, 10, NA, NA)
#' )
#'
#' get_capl_domain_status(x)
#' # [1] "missing protocol"       "complete"               "missing interpretation"
#' # [4] "complete"               "complete"               "incomplete" 
#' # [7] "incomplete"
#'
#' @return returns a character element with a value of "complete", "missing interpretation", "missing protocol" or "incomplete".
get_capl_domain_status <- function(x = NULL) {
  try(
    if(is.null(x)) {
      stop("[CAPL error]: the x argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(x))) {
      stop("[CAPL error]: the x argument must be a data frame or a tibble.")
    } else {
      x[, 2] <- sapply(x[, 2], validate_character)
      x[, -2] <- data.frame(apply(data.frame(x[, -2]), 2, validate_number))
      number_of_columns = ncol(x)
      return(
        unname(
          apply(x, 1, function(x) {
            if(sum(is.na(x[1:number_of_columns])) == 0) {
              "complete"
            } else if(is.na(x[1])) {
              "incomplete"
            } else if(is.na(x[2])) {
              "missing interpretation"
            } else if(sum(is.na(x[3:number_of_columns])) > 0) {
              "missing protocol"
            } else {
              "incomplete"
            }
          })
        )
      )
    }
  )
}

#' Compute an age- and gender-specific CAPL interpretation for a given CAPL protocol or domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param age a numeric element or vector (valid values are between 8 and 12).
#' @param gender a character element or vector (valid values currently include "girl", "g", "female", "f", "boy", "b", "male", "m").
#' @param score a numeric element or vector.
#' @param protocol a character element representing a CAPL protocol (valid values currently include "pacer", "plank", "camsa", "pc"; valid values are 
#' not case-sensitive).
#'
#' @examples
#' get_capl_interpretation(
#'   age = 7:13, 
#'   gender = c("g", "g", "b", "Boy", "m", "f", "Female"), 
#'   score = c(50, 25, 100, 5, 150, 23, 78), 
#'   protocol = "pacer"
#' )
#' # [1] NA            "achieving"   "excelling"   "beginning"   "excelling"   "progressing"
#' # [7] NA
#'
#' @return returns a character element with a value of "beginning", "progressing", "achieving" or "excelling" (if valid) or NA (if not valid).
get_capl_interpretation <- function(age = NA, gender = NA, score = NA, protocol = NA) {
  try(
    if(var(c(length(age), length(gender), length(score))) == 0) {
      return(
        unname(
          apply(data.frame(age, gender, score, rep(tolower(protocol[1]), length(age))), 1, function(x) {
            age <- validate_age(x[1])
            gender <- validate_gender(x[2])
            score <- validate_number(x[3])
            protocol <- as.character(x[4])
            if(sum(is.na(c(age, gender, score, protocol))) > 0 | ! protocol %in% c("pacer", "plank", "camsa", "pc")) {
              return(NA)
            } else {
              if(protocol == "pacer") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(10, 25, 37, 37, 9, 19, 27, 27, 10, 27, 39, 39, 10, 21, 29, 29, 11, 28, 41, 41, 10, 21, 30, 30, 11, 30, 43, 43, 11, 23, 32, 32, 13, 33, 48, 48, 12, 26, 36, 36),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              } else if(protocol == "plank") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(12.4, 72.0, 101.0, 101.0, 24.4, 59.4, 89.3, 89.3, 15.2, 74.9, 103.8, 103.8, 25.2, 61.4, 92.2, 92.2, 18.1, 77.7, 106.7, 106.7, 26.0, 63.4, 95.2, 95.2, 20.9, 80.6, 109.5, 109.5, 26.8, 65.3, 98.2, 98.2, 23.8, 83.4, 112.4, 112.4, 27.6, 67.3, 101.2, 101.2),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              } else if(protocol == "camsa") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(16, 21, 23, 23, 15, 20, 21, 21, 17, 22, 23, 23, 16, 21, 22, 22, 17, 22, 24, 24, 17, 22, 23, 23, 18, 23, 25, 25, 17, 22, 24, 24, 18, 24, 26, 26, 18, 23, 25, 25),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              } else if(protocol == "pc") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(13.4, 19.4, 22.0, 22.0, 13.2, 18.0, 20.3, 20.3, 13.7, 19.9, 22.5, 22.5, 13.7, 18.6, 20.9, 20.9, 14.0, 20.3, 23.0, 23.0, 14.1, 19.1, 21.6, 21.6, 14.3, 20.8, 23.6, 23.6, 14.5, 19.8, 22.3, 22.3, 14.9, 21.6, 24.5, 24.5, 15.2, 20.7, 23.3, 23.3),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              } else {
                lookup <- data.frame(age = NA, gender = NA, bound = NA, interpretation = NA)
              }
              age <- floor(age)
              lookup <- lookup[which(lookup$age == age & lookup$gender == gender),]
              if(score < lookup$bound[lookup$interpretation == "beginning"]) {
                return("beginning")
              } else if(score <= lookup$bound[lookup$interpretation == "progressing"]) {
                return("progressing")
              } else if(score <= lookup$bound[lookup$interpretation == "achieving"]) {
                return("achieving")
              } else if(score > lookup$bound[lookup$interpretation == "excelling"]) {
                return("excelling")
              } else {
                return(NA)
              }
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the age, gender and score arguments must be the same length.")
    }
  )
}