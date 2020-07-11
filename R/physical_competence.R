#' Compute the maximum CAMSA skill + time score for two trials.
#'
#' @description
#' This function computes the maximum CAMSA (Canadian Agility and Movement Skill Assessment) skill + time score for two trials and then divides by 2.8 so
#' that the score is out of 10.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param camsa_score1 A numeric (integer) vector representing the skill + time score from trial 1 (valid values are between 1 and 28).
#' @param camsa_score2 A numeric (integer) vector representing the skill + time score from trial 2 (valid values are between 1 and 28).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' @examples
#' get_camsa_overall_score(
#'   camsa_score1 = c(1, 5, 10, 28, 29), 
#'   camsa_score2 = c(5, 7, 12, NA, 27)
#' )
#'
#' # [1]  5  7 12 NA NA
#'
#' @return Returns a numeric vector with values between 0.36 and 10 (if valid) or NA (if not valid).
get_camsa_overall_score <- function(camsa_score1 = NA, camsa_score2 = NA) {
  try(
    if(var(c(length(camsa_score1), length(camsa_score2))) == 0) {
      return(
        unname(
          apply(data.frame(camsa_score1, camsa_score2), 1, function(x) {
            camsa_score1 <- validate_scale(x[1], 1, 28)
            camsa_score2 <- validate_scale(x[2], 1, 28)
            if(sum(is.na(c(camsa_score1, camsa_score2))) > 0) {
              return(NA)
            } else {
              return(max(camsa_score1, camsa_score2) / 2.8)
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the camsa_skill_score and camsa_time_score arguments must be the same length.")
    }
  )
}

#' Compute the CAMSA time score.
#'
#' @description
#' This function computes the CAMSA (Canadian Agility and Movement Skill Assessment) time score based on the time taken (in seconds) to complete a trial.
#'
#' @export
#'
#' @param camsa_time A numeric vector representing the time taken (in seconds) to complete a CAMSA trial (valid values are > 0).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_number()].
#'
#' @examples
#' get_camsa_time_score(c(14, 12, 30, 25, 0))
#'
#' # [1] 13 14  1  4 NA
#'
#' @return Returns a numeric vector with values between 1 and 14 (if valid) or NA (if not valid).
get_camsa_time_score <- function(camsa_time = NA) {
  return(
    unname(
      sapply(camsa_time, function(x) {
        camsa_time <- validate_number(x)
        if(is.na(camsa_time) | camsa_time <= 0) {
          return(NA)
        } else if(camsa_time > 0 & camsa_time < 14) {
          return(14)
        } else if(camsa_time < 15) {
          return(13)
        } else if(camsa_time < 16) {
          return(12)
        } else if(camsa_time < 17) {
          return(11)
        } else if(camsa_time < 18) {
          return(10)
        } else if(camsa_time < 19) {
          return(9)
        } else if(camsa_time < 20) {
          return(8)
        } else if(camsa_time < 21) {
          return(7)
        } else if(camsa_time < 22) {
          return(6)
        } else if(camsa_time < 24) {
          return(5)
        } else if(camsa_time < 26) {
          return(4)
        } else if(camsa_time < 28) {
          return(3)
        } else if(camsa_time < 30) {
          return(2)
        } else if(camsa_time >= 30) {
          return(1)
        } else {
          return(NA)
        }
      })
    )
  )
}

#' Compute the CAMSA skill + time score.
#'
#' @description
#' This function computes the CAMSA (Canadian Agility and Movement Skill Assessment) skill + time score for a given trial.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param camsa_skill_score A numeric (integer) vector representing the CAMSA skill score (valid values are between 0 and 14).
#' @param camsa_time_score A numeric vector representing the CAMSA time score (valid values are between 1 and 14).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' @examples
#' get_camsa_trial_score(
#'   camsa_skill_score = c(0, 5, 10, 14, 15),
#'   camsa_time_score = c(1, 10, 12, 15, 30)
#' )
#'
#' # [1]  1 15 22 NA NA
#'
#' @return Returns a numeric (integer) vector with values between 1 and 28 (if valid) or NA (if not valid).
get_camsa_trial_score <- function(camsa_skill_score = NA, camsa_time_score = NA) {
  try(
    if(var(c(length(camsa_skill_score), length(camsa_time_score))) == 0) {
      return(
        unname(
          apply(data.frame(camsa_skill_score, camsa_time_score), 1, function(x) {
            camsa_skill_score <- validate_scale(x[1], 0, 14)
            camsa_time_score <- validate_scale(x[2], 1, 14)
            if(sum(is.na(c(camsa_skill_score, camsa_time_score))) > 0) {
              return(NA)
            } else {
              return(sum(camsa_skill_score, camsa_time_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the camsa_skill_score and camsa_time_score arguments must be the same length.")
    }
  )
}

#' Convert PACER shuttle run laps to their equivalent in 20-metre laps.
#'
#' @description 
#' This function converts PACER (Progressive Aerobic Cardiovascular Endurance Run) shuttle run laps to their equivalent in 20-metre laps. If laps are
#' already 20-metre laps, they are returned unless outside the valid range (1-229). This computation is used to compute the PACER score variable
#' (`pacer_score`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param lap_distance A numeric (integer) vector (valid values are 15 or 20).
#' @param laps_run A numeric (integer) vector (if `lap_distance` = 15, valid values are integers between 1 and 298; if `lap_distance` = 20, valid values
#' are integers between 1 and 229).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_integer()] and [validate_scale()].
#'
#' @examples
#' get_pacer_20m_laps(
#'   lap_distance = c(15, 20, NA, "15", 20.5), 
#'   laps_run = rep(100, 5)
#' )
#'
#' # [1]  77 100  NA  77  NA
#'
#' @return Returns a numeric (integer) vector with values between 1 and 229 (if valid) or NA (if not valid).
get_pacer_20m_laps <- function(lap_distance = NA, laps_run = NA) {
  try(
    if(var(c(length(lap_distance), length(laps_run))) == 0) {
      lookup <- data.frame(
        meters_15 = 1:298,
        meters_20 = c(
          1,
          rep(2, 2),
          3:4,
          rep(5, 2),
          6:7,
          rep(8, 2),
          9:11,
          rep(12, 2),
          13:14,
          rep(15, 2),
          16:17,
          rep(18, 2),
          19:21,
          rep(22, 2),
          23:24,
          rep(25, 2),
          26:27,
          rep(28, 2),
          29:31,
          rep(32, 2),
          33:34,
          rep(35, 2),
          36:37,
          rep(38, 2),
          39:40,
          rep(41, 2),
          42:44,
          rep(45, 2),
          46:47,
          rep(48, 2),
          49:50,
          rep(51, 2),
          52:54,
          rep(55, 2),
          56:57,
          rep(58, 2),
          59:60,
          rep(61, 2),
          62:64,
          rep(65, 2),
          66:67,
          rep(68, 2),
          69:71,
          rep(72, 2),
          73:74,
          rep(75, 2),
          76:77,
          rep(78, 2),
          79:81,
          rep(82, 2),
          83:84,
          rep(85, 2),
          86:87,
          rep(88, 2),
          89:91,
          rep(92, 2),
          93,
          rep(94, 2),
          95:97,
          rep(98, 2),
          99:101,
          rep(102, 2),
          103:104,
          rep(105, 2),
          106:107,
          rep(108, 2),
          109:111,
          rep(112, 2),
          113:114,
          rep(115, 2),
          116:117,
          rep(118, 2),
          119:121,
          rep(122, 2),
          123:124,
          rep(125, 2),
          126:127,
          rep(128, 2),
          129:130,
          rep(131, 2),
          132:134,
          rep(135, 2),
          136:137,
          rep(138, 2),
          139:141,
          rep(142, 2),
          143,
          rep(144, 2),
          145:147,
          rep(148, 2),
          149:151,
          rep(152, 2),
          153:154,
          rep(155, 2),
          rep(156, 2),
          157:161,
          rep(162, 2),
          163:164,
          rep(165, 2),
          166:167,
          rep(168, 2),
          169:170,
          rep(171, 2),
          172:174,
          rep(175, 2),
          176:177,
          rep(178, 2),
          179:181,
          rep(182, 2),
          183:184,
          rep(185, 2),
          186:185,
          186,
          188:191,
          rep(192, 2),
          193:194,
          rep(195, 2),
          196:197,
          rep(198, 2),
          199:201,
          200:201,
          203:204,
          rep(205, 2),
          206:207,
          rep(208, 2),
          209:211,
          rep(212, 2),
          213:215,
          rep(216, 2),
          217,
          rep(218, 2),
          219:221,
          rep(222, 2),
          223:224,
          rep(225, 2),
          226:227,
          rep(228, 2),
          229
        )
      )
      return(
        unname(
          apply(data.frame(lap_distance, laps_run), 1, function(x) {
            lap_distance <- validate_integer(x[1])
            laps_run <- validate_integer(x[2])
            if(sum(is.na(c(lap_distance, laps_run))) > 0 | ! lap_distance %in% c(15, 20) | (lap_distance == 15 & is.na(validate_scale(laps_run, 1, 298))) | (lap_distance == 20 & is.na(validate_scale(laps_run, 1, 229)))) {
              return(NA)
            } else if(lap_distance == 15) {
              return(lookup$meters_20[laps_run])
            } else {
              return(laps_run)
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the lap_distance and laps_run arguments must be the same length.")
    }
  )
}

#' Compute a PACER score based on the number of PACER laps run at a 20-metre distance.
#'
#' @description
#' This function computes a PACER (Progressive Aerobic Cardiovascular Endurance Run) score based on the number of PACER laps run at a 20-metre distance.
#' This score is used to compute the physical competence domain score variable (`pc_score`).
#'
#' @export
#'
#' @param pacer_laps_20m A numeric (integer) vector (valid values between 1 and 229).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()] and [validate_integer()].
#'
#' @examples
#' get_pacer_score(c(1, 6, 12, 18, NA, 46, 31, 45.1))
#'
#' # [1]  0  1  2  3 NA  9  6 NA
#'
#' @return Returns a numeric (integer) vector with values between 0 and 10 (if valid) or NA (if not valid).
get_pacer_score <- function(pacer_laps_20m = NA) {
  pacer_laps_20m <- validate_scale(pacer_laps_20m, 1, 229)
  return(
    unname(
      sapply(pacer_laps_20m, function(x) {
        pacer_laps_20m <- validate_integer(x)
        if(is.na(pacer_laps_20m)) {
          return(NA)
        } else if(pacer_laps_20m > 49) {
          return(10)
        } else if(pacer_laps_20m >= 45) {
          return(9)
        } else if(pacer_laps_20m >= 40) {
          return(8)
        } else if(pacer_laps_20m >= 35) {
          return(7)
        } else if(pacer_laps_20m >= 30) {
          return(6)
        } else if(pacer_laps_20m >= 25) {
          return(5)
        } else if(pacer_laps_20m >= 20) {
          return(4)
        } else if(pacer_laps_20m >= 15) {
          return(3)
        } else if(pacer_laps_20m >= 10) {
          return(2)
        } else if(pacer_laps_20m >= 5) {
          return(1)
        } else if(pacer_laps_20m > 0) {
          return(0)
        } else {
          return(NA)
        }
      })
    )
  )
}

#' Compute a physical competence domain score.
#'
#' @description
#' This function computes a physical competence domain score based on the PACER (Progressive Aerobic Cardiovascular Endurance Run), plank and CAMSA 
#' (Canadian Agility and Movement Skill Assessment) scores. If one protocol score is missing or invalid, a weighted domain score will be computed from 
#' the other two protocol scores.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param pacer_score A numeric (integer) vector representing the PACER score (valid values are integers between 0 and 10).
#' @param plank_score a numeric (integer) vector representing the plank score (valid values are integers between 0 and 10).
#' @param camsa_overall_score A numeric vector representing the best CAMSA skill + skill score divided by 2.8 (valid values are between 0.36 and 10).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_scale()].
#'
#' @examples
#' get_pc_score(
#'   pacer_score = c(1, 5, 8, 10, NA),
#'   plank_score = c(4, 5, 5, 6, 9),
#'   camsa_overall_score = c(-1, 0, 6, 4, 3)
#' )
#'
#' # [1]  7.5 10.0 19.0 20.0 18.0
#'
#' @return Returns a numeric vector with values between 0 and 30 (if valid) or NA (if not valid).
get_pc_score <- function(pacer_score = NA, plank_score = NA, camsa_overall_score = NA) {
  try(
    if(var(c(length(pacer_score), length(plank_score), length(camsa_overall_score))) == 0) {
      return(
        unname(
          apply(data.frame(pacer_score, plank_score, camsa_overall_score), 1, function(x) {
            pacer_score <- validate_scale(x[1], 0, 10)
            plank_score <- validate_scale(x[2], 0, 10)
            camsa_overall_score <- validate_scale(x[3], 0, 10)
            if(sum(is.na(c(pacer_score, plank_score, camsa_overall_score))) > 1) {
              return(NA)
            } else if(sum(is.na(c(pacer_score, plank_score, camsa_overall_score))) == 1) {
              return(sum(pacer_score, plank_score, camsa_overall_score, na.rm = TRUE) * 30 / 20)
            } else {
              return(sum(pacer_score, plank_score, camsa_overall_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the pacer_score, plank_score and camsa_overall_score arguments must be the same length.")
    }
  )
}

#' Compute a plank score.
#'
#' @description
#' This function computes a plank score based on the duration of time (in seconds) for which a plank is held. 
#'
#' @export
#'
#' @param plank_time A numeric vector representing time (in seconds).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_number()].
#'
#' @examples
#' get_plank_score(c(120.5, 75.6, 40, 10.99, 90))
#'
#' # [1] 10  6  3  0  8
#'
#' @return Returns a numeric vector with values between 0 and 10 (if valid) or NA (if not valid).
get_plank_score <- function(plank_time = NA) {
  return(
    unname(
      sapply(plank_time, function(x) {
        plank_time <- validate_number(x)
        if(is.na(plank_time)) {
          return(NA)
        } else if(plank_time > 110) {
          return(10)
        } else if(plank_time >= 100) {
          return(9)
        } else if(plank_time >= 90) {
          return(8)
        } else if(plank_time >= 80) {
          return(7)
        } else if(plank_time >= 70) {
          return(6)
        } else if(plank_time >= 60) {
          return(5)
        } else if(plank_time >= 50) {
          return(4)
        } else if(plank_time >= 40) {
          return(3)
        } else if(plank_time >= 30) {
          return(2)
        } else if(plank_time >= 20) {
          return(1)
        } else if(plank_time > 0) {
          return(0)
        } else {
          return(NA)
        }
      })
    )
  )
}