#' Compute a fill in the blanks score.
#'
#' @description
#' This function computes a score (`fill_in_the_blanks_score`) for responses to the fill in the blanks items (story about Sally) in 
#' [the CAPL-2 Questionnaire](https://www.capl-eclp.ca/wp-content/uploads/2018/02/CAPL-2-questionnaire.pdf). This score is used to compute the knowledge 
#' and understanding domain score (`ku_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param pa_is A vector representing a response to the first fill in the blank item (correct answers are 1, 7, "Fun" or "Good").
#' @param pa_is_also A vector representing a response to the second fill in the blank item (correct answers are 1, 7, "Fun" or "Good").
#' @param improve A vector representing a response to the third fill in the blank item (correct answers are 3 or "Endurance").
#' @param increase A vector representing a response to the fourth fill in the blank item (correct answers are 8 or "Strength").
#' @param when_cooling_down A vector representing a response to the fifth fill in the blank item (correct answers are 2 or "Stretches").
#' @param heart_rate A vector representing a response to the sixth fill in the blank item (correct answers are 4 or "Pulse").
#' 
#' @details
#' The following integers represent the responses for the items/arguments in this function:
#' * 1 = Fun
#' * 2 = Stretches
#' * 3 = Endurance
#' * 4 = Pulse
#' * 5 = Breathing
#' * 6 = Flexibility
#' * 7 = Good
#' * 8 = Strength
#' * 9 = Bad
#' * 10 = Sport
#' 
#' Other `capl` functions called by this function include: [get_binary_score()].
#'
#' @examples
#' get_fill_in_the_blanks_score(
#'   pa_is = c(2, 3, "fun", 9),
#'   pa_is_also = c(2, 5, "Fun", 9),
#'   improve = c(1, 3, 10, "Endurance"),
#'   increase = c(2, 3.5, "strength", "strength"),
#'   when_cooling_down = c("stretches", 9, 2, ""),
#'   heart_rate = c(3, 9, 4, "pulse")
#' )
#'
#' # [1] 0 1 3 1
#'
#' @return Returns a numeric (integer) vector with values between 0 and 5 (if valid) or NA (if not valid).
get_fill_in_the_blanks_score <- function(pa_is = NA, pa_is_also = NA, improve = NA, increase = NA, when_cooling_down = NA, heart_rate = NA) {
  try(
    if(var(c(length(pa_is), length(pa_is_also), length(improve), length(increase), length(when_cooling_down), length(heart_rate))) == 0) {
      return(
        unname(
          apply(data.frame(pa_is, pa_is_also, improve, increase, when_cooling_down, heart_rate), 1, function(x) {
            pa_is <- get_binary_score(x[1], c(1, 7, "Fun", "Good"))
            pa_is_also <- get_binary_score(x[2], c(1, 7, "Fun", "Good"))
            improve <- get_binary_score(x[3], c(3, "Endurance"))
            increase <- get_binary_score(x[4], c(8, "Strength"))
            when_cooling_down <- get_binary_score(x[5], c(2, "Stretches"))
            heart_rate <- get_binary_score(x[6], c(4, "Pulse"))
            if(sum(is.na(c(pa_is, pa_is_also, improve, increase, when_cooling_down, heart_rate))) > 1) {
              return(NA)
            } else {
              return(sum(pa_is, pa_is_also, improve, increase, when_cooling_down, heart_rate))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the pa_is, pa_is_also, improve, increase, when_cooling_down and heart_rate arguments must be the same length.")
    }
  )
}

#' Compute a knowledge and understanding domain score. 
#'
#' @description 
#' This function computes a knowledge and understanding domain score (`ku_score`) based on the physical activity guideline (`pa_guideline_score`), 
#' cardiorespiratory fitness means (`crt_means_score`), muscular strength and endurance means (`ms_score`), 
#' sports skill (`sports_skill_score`) and fill in the blanks (`fill_in_the_blanks_score`) scores. If one of the scores is missing or invalid, a weighted
#' domain score will be computed from the other four scores. This score is used to compute the overall physical literacy score (`capl_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param pa_guideline_score A numeric (integer) vector (valid values are between 0 and 1).
#' @param crt_means_score A numeric (integer) vector (valid values are between 0 and 1).
#' @param ms_means_score A numeric (integer) vector (valid values are between 0 and 1).
#' @param sports_skill_score A numeric (integer) vector (valid values are between 0 and 1).
#' @param fill_in_the_blanks_score A numeric (integer) vector (valid values are between 0 and 6).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' @examples
#' get_ku_score(
#'   pa_guideline_score = c(1, 0, 1, 1, NA),
#'   crt_means_score = c(0, 1, "", 2, 1),
#'   ms_means_score = c(1, 1, 1, 0, 0),
#'   sports_skill_score = c(0, 0, 1, 0, 1),
#'   fill_in_the_blanks_score = c(5, 6, 3, 1, 2)
#' )
#'
#' # [1] 7.000000 8.000000 6.666667 2.222222 4.444444
#'
#' @return Returns a numeric vector with values between 0 and 10 (if valid) or NA (if not valid).
get_ku_score <- function(pa_guideline_score = NA, crt_score = NA, ms_means_score = NA, sports_skill_score = NA, fill_in_the_blanks_score = NA) {
  try(
    if(var(c(length(pa_guideline_score), length(crt_means_score), length(ms_means_score), length(sports_skill_score), length(fill_in_the_blanks_score))) == 0) {
      return(
        unname(
          apply(data.frame(pa_guideline_score, crt_means_score, ms_means_score, sports_skill_score, fill_in_the_blanks_score), 1, function(x) {
            pa_guideline_score <- validate_scale(x[1], 0, 1)
            crt_means_score <- validate_scale(x[2], 0, 1)
            ms_means_score <- validate_scale(x[3], 0, 1)
            sports_skill_score <- validate_scale(x[4], 0, 1)
            fill_in_the_blanks_score <- validate_scale(x[5], 0, 6)
            if(sum(is.na(c(pa_guideline_score, crt_means_score, ms_means_score, sports_skill_score, fill_in_the_blanks_score))) > 1) {
              return(NA)
            } else if(sum(is.na(c(pa_guideline_score, crt_means_score, ms_means_score, sports_skill_score, fill_in_the_blanks_score))) == 1) {
              if(is.na(fill_in_the_blanks_score)) {
                denominator <- 4
              } else {
                denominator <- 9
              }
              return(sum(pa_guideline_score, crt_means_score, ms_means_score, sports_skill_score, fill_in_the_blanks_score, na.rm = TRUE) * 10 / denominator)
            } else {
              return(sum(pa_guideline_score, crt_means_score, ms_means_score, sports_skill_score, fill_in_the_blanks_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the pa_guideline_score, crt_means_score, ms_means_score, sports_skill_score and fill_in_the_blanks_score arguments must be the same length.")
    }
  )
}