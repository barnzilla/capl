#' Compute a score for responses to the CSAPPA items 2, 4 and 6.
#'
#' @description
#' This function computes an adequacy score for responses to items 2, 4 and 6 of the Children's Self-Perceptions of Adequacy in and Predilection 
#' for Physical #' Activity (CSAPPA; Hay, 1992) questionnaire as they appear in the CAPL-2 questionnaire. This score is used to compute the motivation
#' and confidence
#' domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param csappa2 a numeric (integer) element or vector representing a response to CSAPPA item 2 (valid values are integers between 1 and 4).
#' @param csappa4 a numeric (integer) element or vector representing a response to CSAPPA item 4 (valid values are integers between 1 and 4).
#' @param csappa6 a numeric (integer) element or vector representing a response to CSAPPA item 6 (valid values are integers between 1 and 4).
#'
#' @details
#' This function calls [validate_questionnaire_item()] to validate the arguments before computing the adequacy score.
#'
#' Valid elements (integers between 1 and 4) represent the following responses:
#' * 1 = REALLY TRUE for me for "some kids" statements
#' * 2 = SORT OF TRUE for me for "some kids" statements
#' * 3 = REALLY TRUE for me for "other kids" statements
#' * 4 = SORT OF TRUE for me for "other kids" statements
#'
#' @examples
#' get_adequacy_score(
#'   csappa2 = c(1:3, 0),
#'   csappa4 = c(4, 2, 1, "3"),
#'   csappa6 = c(4, 4, 2, 2)
#' )
#'
#' # [1] 4.9 4.8 4.9  NA
#'
#' @return returns a numeric element between 1.8 and 7.5 (if valid) or NA (if not valid).
get_adequacy_score <- function(csappa2 = NA, csappa4 = NA, csappa6 = NA) {
  try(
    if(var(c(length(csappa2), length(csappa4), length(csappa6))) == 0) {
      return(
        unname(
          apply(data.frame(csappa2, csappa4, csappa6), 1, function(x) {
            csappa2 <- validate_questionnaire_item(x[1], 1, 4)
            csappa4 <- validate_questionnaire_item(x[2], 1, 4)
            csappa6 <- validate_questionnaire_item(x[3], 1, 4)
            if(sum(is.na(c(csappa2, csappa4, csappa6))) > 0) {
              return(NA)
            } else {
              lookup <- c(2.5, 1.8, 0.6, 1.2)
              return(sum(lookup[csappa2], lookup[csappa4], lookup[csappa6]))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the csappa2, csappa4 and csappa6 arguments must be the same length.")
    }
  )
}

#' Compute a score for responses to BREQ items 1-3.
#'
#' @description
#' This function computes an intrinsic motivation score for responses to items 1-3 of the the Behavioral Regulation in Exercise Questionnaire (BREQ) as they
#' appear in the CAPL-2 questionnaire. This score is used to compute the motivation and confidence domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param why_are_you_active1 a numeric (integer) element or vector representing a response to BREQ item 1 (valid values are integers between 1 and 5).
#' @param why_are_you_active2 a numeric (integer) element or vector representing a response to BREQ item 2 (valid values are integers between 1 and 5).
#' @param why_are_you_active3 a numeric (integer) element or vector representing a response to BREQ item 3 (valid values are integers between 1 and 5).
#' 
#' @details
#' This function calls [validate_questionnaire_item()] to validate the arguments before computing the intrinsic motivation score.
#'
#' Valid elements (integers between 1 and 5) represent the following responses:
#' * 1 = Not true for me
#' * 2 = Not really true for me
#' * 3 = Sometimes true for me
#' * 4 = Often true for me
#' * 5 = Very true for me
#' 
#' @examples
#' get_intrinsic_motivation_score(
#'   why_are_you_active1 = c(4, 3, 6, 5, "2"),
#'   why_are_you_active2 = c(1:5),
#'   why_are_you_active3 = c(1, 5, 4, 3, 3)
#' )
#'
#' # [1]  3  5 NA  6  5
#'
#' @return returns a numeric element between 1.5 and 7.5 (if valid) or NA (if not valid).
get_intrinsic_motivation_score <- function(why_are_you_active1 = NA, why_are_you_active2 = NA, why_are_you_active3 = NA) {
  try(
    if(var(c(length(why_are_you_active1), length(why_are_you_active2), length(why_are_you_active3))) == 0) {
      return(
        unname(
          apply(data.frame(why_are_you_active1, why_are_you_active2, why_are_you_active3), 1, function(x) {
            why_are_you_active1 <- validate_questionnaire_item(x[1], 1, 5)
            why_are_you_active2 <- validate_questionnaire_item(x[2], 1, 5)
            why_are_you_active3 <- validate_questionnaire_item(x[3], 1, 5)
            if(sum(is.na(c(why_are_you_active1, why_are_you_active2, why_are_you_active3))) > 0) {
              return(NA)
            } else {
              return(sum(why_are_you_active1, why_are_you_active2, why_are_you_active3) / 2)
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the why_are_you_active1, why_are_you_active2 and why_are_you_active3 arguments must be the same length.")
    }
  )
}

#' Compute a motivation and confidence domain score based on the step and self-reported physical activity scores. 
#'
#' @description Compute a motivation and confidence domain score based on the predilection, adequacy, intrinsic motivation and physical activity 
#' competence scores. If one of the scores is missing or invalid, a weighted domain score will be computed from the other three scores.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param predilection_score a numeric element or vector (valid values are between 1.8 and 7.5).
#' @param adequacy_score a numeric element or vector (valid values are between 1.8 and 7.5).
#' @param intrinsic_motivation_score a numeric element or vector (valid values are between 1.5 and 7.5).
#' @param pa_competence_score a numeric element or vector (valid values are between 1.5 and 7.5).
#'
#' @details
#' This function calls [validate_number()] and [validate_protocol_score()] to ensure that the computed score is valid.
#'
#' @examples
#' get_mc_score(
#'   predilection_score = c(7, 7.5, 5, 8, 4),
#'   adequacy_score = c(NA, 5, 3, 1, 4),
#'   intrinsic_motivation_score = c(5, 7.5, 4, 2, 3.5),
#'   pa_competence_score = c(6, 3, 6, 7, 2)
#' )
#'
#' # [1] 24.0 23.0 18.0   NA 13.5
#'
#' @return returns a numeric element between 0 and 30 (if valid) or NA (if not valid).
get_mc_score <- function(predilection_score = NA, adequacy_score = NA, intrinsic_motivation_score = NA, pa_competence_score = NA) {
  try(
    if(var(c(length(predilection_score), length(adequacy_score), length(intrinsic_motivation_score), length(pa_competence_score))) == 0) {
      return(
        unname(
          apply(data.frame(predilection_score, adequacy_score, intrinsic_motivation_score, pa_competence_score), 1, function(x) {
            predilection_score <- validate_protocol_score(x[1], "csappa")
            adequacy_score <- validate_protocol_score(x[2], "csappa")
            intrinsic_motivation_score <- validate_protocol_score(x[3], "breq")
            pa_competence_score <- validate_protocol_score(x[4], "breq")
            if(sum(is.na(c(predilection_score, adequacy_score, intrinsic_motivation_score, pa_competence_score))) > 1) {
              return(NA)
            } else if(sum(is.na(c(predilection_score, adequacy_score, intrinsic_motivation_score, pa_competence_score))) == 1) {
              return(sum(predilection_score, adequacy_score, intrinsic_motivation_score, pa_competence_score, na.rm = TRUE) * 30 / 22.5)
            } else {
              return(sum(predilection_score, adequacy_score, intrinsic_motivation_score, pa_competence_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the predilection_score, adequacy_score, intrinsic_motivation_score and pa_competence_score arguments must be the same length.")
    }
  )
}

#' Compute a score for responses to BREQ items 4-6.
#'
#' @description
#' This function computes an intrinsic motivation score for responses to items 4-6 of the the Behavioral Regulation in Exercise Questionnaire (BREQ) as they
#' appear in the CAPL-2 questionnaire. This score is used to compute the motivation and confidence domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param feelings_about_pa1 a numeric (integer) element or vector representing a response to BREQ item 4 (valid values are integers between 1 and 5).
#' @param feelings_about_pa2 a numeric (integer) element or vector representing a response to BREQ item 5 (valid values are integers between 1 and 5).
#' @param feelings_about_pa3 a numeric (integer) element or vector representing a response to BREQ item 6 (valid values are integers between 1 and 5).
#' 
#' @details
#' This function calls [validate_questionnaire_item()] to validate the arguments before computing the intrinsic motivation score.
#'
#' Valid elements (integers between 1 and 5) represent the following responses:
#' * 1 = Not true for me
#' * 2 = Not really true for me
#' * 3 = Sometimes true for me
#' * 4 = Often true for me
#' * 5 = Very true for me
#' 
#' @examples
#' get_pa_competence_score(
#'   feelings_about_pa1 = c(4, 3, 6, 5, "2"),
#'   feelings_about_pa2 = c(1:5),
#'   feelings_about_pa3 = c(1, 5, 4, 3, 3)
#' )
#'
#' # [1]  3  5 NA  6  5
#'
#' @return returns a numeric element between 1.5 and 7.5 (if valid) or NA (if not valid).
get_pa_competence_score <- function(feelings_about_pa1 = NA, feelings_about_pa2 = NA, feelings_about_pa3 = NA) {
  try(
    if(var(c(length(feelings_about_pa1), length(feelings_about_pa2), length(feelings_about_pa3))) == 0) {
      return(
        unname(
          apply(data.frame(feelings_about_pa1, feelings_about_pa2, feelings_about_pa3), 1, function(x) {
            feelings_about_pa1 <- validate_questionnaire_item(x[1], 1, 5)
            feelings_about_pa2 <- validate_questionnaire_item(x[2], 1, 5)
            feelings_about_pa3 <- validate_questionnaire_item(x[3], 1, 5)
            if(sum(is.na(c(feelings_about_pa1, feelings_about_pa2, feelings_about_pa3))) > 0) {
              return(NA)
            } else {
              return(sum(feelings_about_pa1, feelings_about_pa2, feelings_about_pa3) / 2)
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the feelings_about_pa1, feelings_about_pa2 and feelings_about_pa3 arguments must be the same length.")
    }
  )
}

#' Compute a score for responses to the CSAPPA items 1, 3 and 5.
#'
#' @description
#' This function computes a predilection score for responses to items 1, 3 and 5 of the Children's Self-Perceptions of Adequacy in and Predilection 
#' for Physical Activity (CSAPPA; Hay, 1992) questionnaire as they in the CAPL-2 questionnaire. This score is used to compute the motivation and confidence
#' domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param csappa1 a numeric (integer) element or vector representing a response to CSAPPA item 1 (valid values are integers between 1 and 4).
#' @param csappa3 a numeric (integer) element or vector representing a response to CSAPPA item 3 (valid values are integers between 1 and 4).
#' @param csappa5 a numeric (integer) element or vector representing a response to CSAPPA item 5 (valid values are integers between 1 and 4).
#' 
#' @details
#' This function calls [validate_questionnaire_item()] to validate the arguments before computing the predilection score.
#'
#' Valid elements (integers between 1 and 4) represent the following responses:
#' * 1 = REALLY TRUE for me for "some kids" statements
#' * 2 = SORT OF TRUE for me for "some kids" statements
#' * 3 = REALLY TRUE for me for "other kids" statements
#' * 4 = SORT OF TRUE for me for "other kids" statements
#' 
#' @examples
#' get_predilection_score(
#'   csappa1 = c(1:3, 0),
#'   csappa3 = c(4, 2, 1, "3"),
#'   csappa5 = c(4, 4, 2, 2)
#' )
#'
#' # [1] 4.2 4.2 4.3  NA
#'
#' @return returns a numeric element between 1.8 and 7.5 (if valid) or NA (if not valid).
get_predilection_score <- function(csappa1 = NA, csappa3 = NA, csappa5 = NA) {
  try(
    if(var(c(length(csappa1), length(csappa3), length(csappa5))) == 0) {
      return(
        unname(
          apply(data.frame(csappa1, csappa3, csappa5), 1, function(x) {
            csappa1 <- validate_questionnaire_item(x[1], 1, 4)
            csappa3 <- validate_questionnaire_item(x[2], 1, 4)
            csappa5 <- validate_questionnaire_item(x[3], 1, 4)
            if(sum(is.na(c(csappa1, csappa3, csappa5))) > 0) {
              return(NA)
            } else {
              lookup <- c(0.6, 1.2, 2.5, 1.8)
              return(sum(lookup[csappa1], lookup[csappa3], lookup[csappa5]))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the csappa1, csappa3 and csappa5 arguments must be the same length.")
    }
  )
}