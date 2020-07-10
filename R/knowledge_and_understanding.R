#' Compute a score for responses to the fill in the blanks items in the CAPL-2 questionnaire.
#'
#' @description
#' This function computes a score for responses to the fill in the blanks items (story about Sally) in the CAPL-2 questionnaire. This score is used to
#' compute the knowledge and understanding  domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param pa_is an element or vector representing a response to the first fill in the blank item (correct answers are 1, 7, "Fun" or "Good").
#' @param pa_is_also an element or vector representing a response to the second fill in the blank item (correct answers are 1, 7, "Fun" or "Good").
#' @param improve an element or vector representing a response to the third fill in the blank item (correct answers are 3 or "Endurance").
#' @param increase an element or vector representing a response to the fourth fill in the blank item (correct answers are 8 or "Strength").
#' @param when_cooling_down an element or vector representing a response to the fifth fill in the blank item (correct answers are 2 or "Stretches").
#' @param heart_rate an element or vector representing a response to the sixth fill in the blank item (correct answers are 4 or "Pulse").
#' 
#' @details
#' This function calls [get_binary_score()] to score the arguments before computing the fill in the blanks score.
#'
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
#' @return returns a numeric (integer) element between 0 and 5 (if valid) or NA (if not valid).
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
#' @description Compute a knowledge and understanding domain score based on the physical activity guideline, cardiorespiratory fitness means, muscular
#' strength and endurance means, sports skill and fill in the blanks scores. If one of the scores is missing or invalid, a weighted domain score will be 
#' computed from the other four scores.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param pa_guideline_score a numeric (integer) element or vector (valid values are between 0 and 1).
#' @param cardiorespiratory_fitness_means_score a numeric (integer) element or vector (valid values are between 0 and 1).
#' @param muscular_strength_means_score a numeric (integer) element or vector (valid values are between 0 and 1).
#' @param sports_skill_score a numeric (integer) element or vector (valid values are between 0 and 1).
#' @param fill_in_the_blanks_score a numeric (integer) element or vector (valid values are between 0 and 6).
#'
#' @details
#' This function calls [validate_number()], [validate_integer()] and [validate_protocol_score()] to ensure that the computed score is valid.
#'
#' @examples
#' get_ku_score(
#'   pa_guideline_score = c(1, 0, 1, 1, NA),
#'   cardiorespiratory_fitness_means_score = c(0, 1, "", 2, 1),
#'   muscular_strength_means_score = c(1, 1, 1, 0, 0),
#'   sports_skill_score = c(0, 0, 1, 0, 1),
#'   fill_in_the_blanks_score = c(5, 6, 3, 1, 2)
#' )
#'
#' # [1] 7.000000 8.000000 6.666667 2.222222 4.444444
#'
#' @return returns a numeric element between 0 and 10 (if valid) or NA (if not valid).
get_ku_score <- function(pa_guideline_score = NA, cardiorespiratory_fitness_means_score = NA, muscular_strength_means_score = NA, sports_skill_score = NA, fill_in_the_blanks_score = NA) {
  try(
    if(var(c(length(pa_guideline_score), length(cardiorespiratory_fitness_means_score), length(muscular_strength_means_score), length(sports_skill_score), length(fill_in_the_blanks_score))) == 0) {
      return(
        unname(
          apply(data.frame(pa_guideline_score, cardiorespiratory_fitness_means_score, muscular_strength_means_score, sports_skill_score, fill_in_the_blanks_score), 1, function(x) {
            pa_guideline_score <- validate_protocol_score(x[1], "pa_guideline")
            cardiorespiratory_fitness_means_score <- validate_protocol_score(x[2], "cardiorespiratory_fitness_means")
            muscular_strength_means_score <- validate_protocol_score(x[3], "muscular_strength_means")
            sports_skill_score <- validate_protocol_score(x[4], "sports_skill")
            fill_in_the_blanks_score <- validate_protocol_score(x[5], "fill_in_the_blanks")
            if(sum(is.na(c(pa_guideline_score, cardiorespiratory_fitness_means_score, muscular_strength_means_score, sports_skill_score, fill_in_the_blanks_score))) > 1) {
              return(NA)
            } else if(sum(is.na(c(pa_guideline_score, cardiorespiratory_fitness_means_score, muscular_strength_means_score, sports_skill_score, fill_in_the_blanks_score))) == 1) {
              if(is.na(fill_in_the_blanks_score)) {
                denominator <- 4
              } else {
                denominator <- 9
              }
              return(sum(pa_guideline_score, cardiorespiratory_fitness_means_score, muscular_strength_means_score, sports_skill_score, fill_in_the_blanks_score, na.rm = TRUE) * 10 / denominator)
            } else {
              return(sum(pa_guideline_score, cardiorespiratory_fitness_means_score, muscular_strength_means_score, sports_skill_score, fill_in_the_blanks_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the pa_guideline_score, cardiorespiratory_fitness_means_score, muscular_strength_means_score, sports_skill_score and fill_in_the_blanks_score arguments must be the same length.")
    }
  )
}