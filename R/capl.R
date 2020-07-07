#' The main function in this package that computes CAPL scores and interpretations from raw data.
#'
#' @export
#'
#' @param raw_data a data frame or tibble of raw CAPL data.
#'
#' @return returns a data frame of CAPL scores and interpretations.
get_capl <- function(raw_data = NULL) {
  try(
    if(is.null(raw_data)) {
      stop("[CAPL error]: the raw_data argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(raw_data))) {
      stop("[CAPL error]: the raw_data argument must be a data frame or a tibble.")
    }
    else {
      colnames(raw_data) <- tolower(colnames(raw_data))
      if(! "age" %in% colnames(raw_data)) {
        raw_data$age <- rep(NA, nrow(raw_data))
      }
      if(! "gender" %in% colnames(raw_data)) {
        raw_data$gender <- rep(NA, nrow(raw_data))
      }
      pacer_laps_20m <- get_pacer_20m_laps(raw_data$pacer_lap_distance, raw_data$pacer_laps)
      pacer_score <- get_pacer_score(pacer_laps_20m)
      pacer_interpretation <- get_interpretation(raw_data$age, raw_data$gender, pacer_laps_20m, "pacer")
      plank_score <- get_plank_score(raw_data$plank_time)
      plank_interpretation <- get_interpretation(raw_data$age, raw_data$gender, raw_data$plank_time, "plank")
      camsa_time_score1 <- get_camsa_time_score(raw_data$camsa_time1)
      camsa_time_score2 <- get_camsa_time_score(raw_data$camsa_time2)
      camsa_score1 <- get_camsa_score(raw_data$camsa_skill_score1, camsa_time_score1)
      camsa_score2 <- get_camsa_score(raw_data$camsa_skill_score2, camsa_time_score2)
      camsa_overall_score <- get_camsa_overall_score(camsa_score1, camsa_score2)
      camsa_interpretation <- get_interpretation(raw_data$age, raw_data$gender, camsa_overall_score, "camsa")
      pc_score <- get_pc_score(pacer_score, plank_score, camsa_overall_score)
      pc_interpretation <- get_interpretation(raw_data$age, raw_data$gender, pc_score, "pc")
      return(
        data.frame(
          age = raw_data$age,
          gender = raw_data$gender,
          pacer_laps_20m,
          pacer_score,
          pacer_interpretation,
          plank_score,
          plank_interpretation,
          camsa_time_score1,
          camsa_time_score2,
          camsa_score1,
          camsa_score2,
          camsa_overall_score,
          camsa_interpretation,
          pc_score,
          pc_interpretation
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
#' @param protocol a character element representing a CAPL protocol (valid values currently include "pacer", "plank", "camsa", "pc").
#'
#' @examples
#' get_interpretation(
#'   age = 7:13, 
#'   gender = c("g", "g", "b", "Boy", "m", "f", "Female"), 
#'   score = c(50, 25, 100, 5, 150, 23, 78), 
#'   protocol = "pacer"
#' )
#' # [1] NA            "achieving"   "excelling"   "beginning"   "excelling"   "progressing"
#' # [7] NA
#' @return returns a character element with a value of "beginning", "progressing", "achieving" or "excelling" (if valid) or NA (if not valid).
get_interpretation <- function(age = NA, gender = NA, score = NA, protocol = NA) {
  try(
    if(var(c(length(age), length(gender), length(score))) == 0) {
      return(
        unname(
          apply(data.frame(age, gender, score, rep(protocol[1], length(age))), 1, function(x) {
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
              }
              if(protocol == "plank") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(12.4, 72.0, 101.0, 101.0, 24.4, 59.4, 89.3, 89.3, 15.2, 74.9, 103.8, 103.8, 25.2, 61.4, 92.2, 92.2, 18.1, 77.7, 106.7, 106.7, 26.0, 63.4, 95.2, 95.2, 20.9, 80.6, 109.5, 109.5, 26.8, 65.3, 98.2, 98.2, 23.8, 83.4, 112.4, 112.4, 27.6, 67.3, 101.2, 101.2),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              }
              if(protocol == "camsa") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(16, 21, 23, 23, 15, 20, 21, 21, 17, 22, 23, 23, 16, 21, 22, 22, 17, 22, 24, 24, 17, 22, 23, 23, 18, 23, 25, 25, 17, 22, 24, 24, 18, 24, 26, 26, 18, 23, 25, 25),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              }
              if(protocol == "pc") {
                lookup <- data.frame(
                  age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
                  gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
                  bound = c(13.4, 19.4, 22.0, 22.0, 13.2, 18.0, 20.3, 20.3, 13.7, 19.9, 22.5, 22.5, 13.7, 18.6, 20.9, 20.9, 14.0, 20.3, 23.0, 23.0, 14.1, 19.1, 21.6, 21.6, 14.3, 20.8, 23.6, 23.6, 14.5, 19.8, 22.3, 22.3, 14.9, 21.6, 24.5, 24.5, 15.2, 20.7, 23.3, 23.3),
                  interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
                )
              }
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