#' Compute an adequacy score.
#'
#' @description
#' This function computes an adequacy score (`adequacy_score`) for responses to items 2, 4 and 6 of the CSAPPA (Children's Self-Perceptions of Adequacy in 
#' and Predilection for Physical Activity; Hay, 1992) questionnaire as they appear in the CAPL-2 Questionnaire. This score is used to compute the motivation
#' and confidence domain score (`mc_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param csappa2 A numeric (integer) vector representing a response to CSAPPA item 2 (valid values are integers between 1 and 4).
#' @param csappa4 A numeric (integer) vector representing a response to CSAPPA item 4 (valid values are integers between 1 and 4).
#' @param csappa6 A numeric (integer) vector representing a response to CSAPPA item 6 (valid values are integers between 1 and 4).
#'
#' @details
#' Valid values (integers between 1 and 4) represent the following responses:
#' * 1 = REALLY TRUE for me for "some kids" statements
#' * 2 = SORT OF TRUE for me for "some kids" statements
#' * 3 = REALLY TRUE for me for "other kids" statements
#' * 4 = SORT OF TRUE for me for "other kids" statements
#'
#' Other `capl` functions called by this function include: [validate_scale()].
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
#' @return Returns a numeric vector with values between 1.8 and 7.5 (if valid) or NA (if not valid).
get_adequacy_score <- function(csappa2 = NA, csappa4 = NA, csappa6 = NA) {
  try(
    if(var(c(length(csappa2), length(csappa4), length(csappa6))) == 0) {
      return(
        unname(
          apply(data.frame(csappa2, csappa4, csappa6), 1, function(x) {
            csappa2 <- validate_scale(x[1], 1, 4)
            csappa4 <- validate_scale(x[2], 1, 4)
            csappa6 <- validate_scale(x[3], 1, 4)
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

#' Compute an intrinsic motivation score.
#'
#' @description
#' This function computes an intrinsic motivation score (`intrinsic_motivation_score`) for responses to items 1-3 of the the Behavioral Regulation in
#' Exercise Questionnaire (BREQ) as they appear in the CAPL-2 Questionnaire. This score is used to compute the motivation and confidence domain score 
#' (`mc_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param why_are_you_active1 A numeric (integer) vector representing a response to BREQ item 1 (valid values are integers between 1 and 5).
#' @param why_are_you_active2 a numeric (integer) vector representing a response to BREQ item 2 (valid values are integers between 1 and 5).
#' @param why_are_you_active3 a numeric (integer) vector representing a response to BREQ item 3 (valid values are integers between 1 and 5).
#' 
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' Valid values (integers between 1 and 5) represent the following responses:
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
#' @return Returns a numeric vector with values between 1.5 and 7.5 (if valid) or NA (if not valid).
get_intrinsic_motivation_score <- function(why_are_you_active1 = NA, why_are_you_active2 = NA, why_are_you_active3 = NA) {
  try(
    if(var(c(length(why_are_you_active1), length(why_are_you_active2), length(why_are_you_active3))) == 0) {
      return(
        unname(
          apply(data.frame(why_are_you_active1, why_are_you_active2, why_are_you_active3), 1, function(x) {
            why_are_you_active1 <- validate_scale(x[1], 1, 5)
            why_are_you_active2 <- validate_scale(x[2], 1, 5)
            why_are_you_active3 <- validate_scale(x[3], 1, 5)
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

#' Compute a motivation and confidence domain score. 
#'
#' @description 
#' This function computes a motivation and confidence domain score (`mc_score`) based on the predilection (`predilection_score`), adequacy
#' (`adequacy_score`), intrinsic motivation (`intrinsic_motivation_score`) and physical activity competence (`pa_competence_score`) scores. If one of the
#' scores is missing or invalid, a weighted domain score will be computed from the other three scores. This score is used to compute the overall physical
#' literacy score (`capl_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param predilection_score A numeric vector (valid values are between 1.8 and 7.5).
#' @param adequacy_score A numeric vector (valid values are between 1.8 and 7.5).
#' @param intrinsic_motivation_score A numericvector (valid values are between 1.5 and 7.5).
#' @param pa_competence_score A numeric vector (valid values are between 1.5 and 7.5).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_number()].
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
#' @return Returns a numeric vector with values between 0 and 30 (if valid) or NA (if not valid).
get_mc_score <- function(predilection_score = NA, adequacy_score = NA, intrinsic_motivation_score = NA, pa_competence_score = NA) {
  try(
    if(var(c(length(predilection_score), length(adequacy_score), length(intrinsic_motivation_score), length(pa_competence_score))) == 0) {
      return(
        unname(
          apply(data.frame(predilection_score, adequacy_score, intrinsic_motivation_score, pa_competence_score), 1, function(x) {
            #predilection_score <- validate_protocol_score(x[1], "csappa")
			predilection_score <- validate_number(x[1])
			predilection_score <- ifelse(is.na(predilection_score) | predilection_score < 1.8 | predilection_score > 7.5, NA, predilection_score)
            adequacy_score <- validate_number(x[2])
			adequacy_score <- ifelse(is.na(adequacy_score) | adequacy_score < 1.8 | adequacy_score > 7.5, NA, adequacy_score)
            intrinsic_motivation_score <- validate_number(x[3])
			intrinsic_motivation_score <- ifelse(is.na(intrinsic_motivation_score) | intrinsic_motivation_score < 1.5 | intrinsic_motivation_score > 7.5, NA, intrinsic_motivation_score)
			pa_competence_score <- validate_number(x[4])
			pa_competence_score <- ifelse(is.na(pa_competence_score) | pa_competence_score < 1.5 | pa_competence_score > 7.5, NA, pa_competence_score)
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

#' Compute a physical activity competence score.
#'
#' @description
#' This function computes a physical activity competence score (`pa_competence_score`) for responses to items 4-6 of the the Behavioral Regulation in
#' Exercise Questionnaire (BREQ) as they appear in the CAPL-2 Questionnaire. This score is used to compute the motivation and confidence domain score 
#' (`mc_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param feelings_about_pa1 A numeric (integer) vector representing a response to BREQ item 4 (valid values are integers between 1 and 5).
#' @param feelings_about_pa2 A numeric (integer) vector representing a response to BREQ item 5 (valid values are integers between 1 and 5).
#' @param feelings_about_pa3 A numeric (integer) vector representing a response to BREQ item 6 (valid values are integers between 1 and 5).
#' 
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
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
#' @return Returns a numeric vector with values between 1.5 and 7.5 (if valid) or NA (if not valid).
get_pa_competence_score <- function(feelings_about_pa1 = NA, feelings_about_pa2 = NA, feelings_about_pa3 = NA) {
  try(
    if(var(c(length(feelings_about_pa1), length(feelings_about_pa2), length(feelings_about_pa3))) == 0) {
      return(
        unname(
          apply(data.frame(feelings_about_pa1, feelings_about_pa2, feelings_about_pa3), 1, function(x) {
            feelings_about_pa1 <- validate_scale(x[1], 1, 5)
            feelings_about_pa2 <- validate_scale(x[2], 1, 5)
            feelings_about_pa3 <- validate_scale(x[3], 1, 5)
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

#' Compute a predilection score.
#'
#' @description
#' This function computes a predilection score (`predilection_score`) for responses to items 1, 3 and 5 of the CSAPPA (Children's Self-Perceptions of
#' Adequacy in and Predilection for Physical Activity; Hay, 1992) questionnaire as they appear in the CAPL-2 Questionnaire. This score is used to compute
#' the motivation and confidence domain score (`mc_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param csappa1 A numeric (integer) vector representing a response to CSAPPA item 1 (valid values are integers between 1 and 4).
#' @param csappa3 A numeric (integer) vector representing a response to CSAPPA item 3 (valid values are integers between 1 and 4).
#' @param csappa5 A numeric (integer) vector representing a response to CSAPPA item 5 (valid values are integers between 1 and 4).
#' 
#' @details
#' Valid values (integers between 1 and 4) represent the following responses:
#' * 1 = REALLY TRUE for me for "some kids" statements
#' * 2 = SORT OF TRUE for me for "some kids" statements
#' * 3 = REALLY TRUE for me for "other kids" statements
#' * 4 = SORT OF TRUE for me for "other kids" statements
#' 
#' Other `capl` functions called by this function include: [validate_scale()].
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
#' @return Returns a numeric vector with values between 1.8 and 7.5 (if valid) or NA (if not valid).
get_predilection_score <- function(csappa1 = NA, csappa3 = NA, csappa5 = NA) {
  try(
    if(var(c(length(csappa1), length(csappa3), length(csappa5))) == 0) {
      return(
        unname(
          apply(data.frame(csappa1, csappa3, csappa5), 1, function(x) {
            csappa1 <- validate_scale(x[1], 1, 4)
            csappa3 <- validate_scale(x[2], 1, 4)
            csappa5 <- validate_scale(x[3], 1, 4)
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