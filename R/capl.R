#' Compute all CAPL-2 scores and interpretations at once.
#'
#' This function is the main function in the `capl` package. It is a wrapper function that calls all other `capl` functions to compute all CAPL-2 scores and
#' interpretations from raw data at once. If required CAPL-2 variables are missing, the function will create the variables and set values for these
#' variables to NA so the function can proceed.
#'
#' @export
#'
#' @param raw_data A data frame of raw CAPL-2 data.
#' @param sort An optional character vector representing how the variables in the returned data frame are to be sorted (valid values are "asis, "abc"
#' and "zyx"; valid values are not case-sensitive). This argument is set to "asis" by default.
#' @param version An optional numeric (integer) vector representing the version of CAPL. This argument is set to 2 by default. If set to 1, [get_fill_in_the_blanks_score()]
#' will ignore the when_cooling_down parameter and re-weight the score so that it's out of six.
#'
#' @details
#' Other `capl` functions called by this function include: [get_missing_capl_variables()], [get_pacer_20m_laps()], [get_pacer_score()],
#' [get_capl_interpretation()], [get_plank_score()], [get_camsa_time_score()], [get_camsa_skill_time_score()], [get_camsa_score()], [get_pc_score()],
#' [get_capl_domain_status()], [get_pedometer_wear_time()], [validate_steps()], [get_step_average()], [get_step_score()],
#' [get_self_report_pa_score()], [get_db_score()], [get_predilection_score()], [get_adequacy_score()],
#' [get_intrinsic_motivation_score()], [get_pa_competence_score()], [get_mc_score()], [get_binary_score()], [get_fill_in_the_blanks_score()],
#' [get_ku_score()] and [get_capl_score()]
#'
#' @examples
#' get_capl(raw_data)
#'
#' @return Returns a merged data frame of raw data and CAPL-2 scores and interpretations.
get_capl <- function(raw_data = NULL, sort = "asis", version = 2) {
  try(
    if(is.null(raw_data)) {
      stop("[CAPL error]: the raw_data argument is missing.")
    }
    else if(! isTRUE("data.frame" %in% class(raw_data))) {
      stop("[CAPL error]: the raw_data argument must be a data frame.")
    }
    else {
      sort <- tolower(sort[1])
      # Add required CAPL-2 raw variables if missing
      raw_data <- get_missing_capl_variables(raw_data)

      # Physical competence computations
      raw_data$pacer_laps_20m <- get_pacer_20m_laps(raw_data$pacer_lap_distance, raw_data$pacer_laps)
      raw_data$pacer_score <- get_pacer_score(raw_data$pacer_laps_20m)
      raw_data$pacer_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$pacer_laps_20m, "pacer")
      raw_data$plank_score <- get_plank_score(raw_data$plank_time)
      raw_data$plank_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$plank_time, "plank")
      raw_data$camsa_time_score1 <- get_camsa_time_score(raw_data$camsa_time1)
      raw_data$camsa_time_score2 <- get_camsa_time_score(raw_data$camsa_time2)
      raw_data$camsa_skill_time_score1 <- get_camsa_skill_time_score(raw_data$camsa_skill_score1, raw_data$camsa_time_score1)
      raw_data$camsa_skill_time_score2 <- get_camsa_skill_time_score(raw_data$camsa_skill_score2, raw_data$camsa_time_score2)
      raw_data$camsa_score <- get_camsa_score(raw_data$camsa_skill_time_score1, raw_data$camsa_skill_time_score2)
      raw_data$camsa_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$camsa_score, "camsa")
      raw_data$pc_score <- get_pc_score(raw_data$pacer_score, raw_data$plank_score, raw_data$camsa_score)
      raw_data$pc_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$pc_score, "pc")
      raw_data$pc_status <- get_capl_domain_status(raw_data, "pc")

      # Daily behaviour computations
      step_df <- get_step_average(raw_data)
      raw_data$valid_days <- step_df$valid_days
      raw_data$step_average <- step_df$step_average
      raw_data$step_score <- get_step_score(raw_data$step_average)
      raw_data$step_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$step_average, "steps")
      raw_data$self_report_pa_score <- get_self_report_pa_score(raw_data$self_report_pa)
      raw_data$db_score <- get_db_score(raw_data$step_score, raw_data$self_report_pa_score)
      raw_data$db_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$db_score, "db")
      raw_data$db_status <- get_capl_domain_status(raw_data, "db")

      # Motivation and confidence computations
      raw_data$predilection_score <- get_predilection_score(raw_data$csappa1, raw_data$csappa3, raw_data$csappa5)
      raw_data$adequacy_score <- get_adequacy_score(raw_data$csappa2, raw_data$csappa4, raw_data$csappa6)
      raw_data$intrinsic_motivation_score <- get_intrinsic_motivation_score(raw_data$why_active1, raw_data$why_active2, raw_data$why_active3)
      raw_data$pa_competence_score <- get_pa_competence_score(raw_data$feelings_about_pa1, raw_data$feelings_about_pa2, raw_data$feelings_about_pa3)
      raw_data$mc_score <- get_mc_score(raw_data$predilection_score, raw_data$adequacy_score, raw_data$intrinsic_motivation_score, raw_data$pa_competence_score)
      raw_data$mc_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$mc_score, "mc")
      raw_data$mc_status <- get_capl_domain_status(raw_data, "mc")

      # Knowledge and understanding computations
      raw_data$pa_guideline_score <- get_binary_score(raw_data$pa_guideline, c(ifelse(! is.na(version) & version == 1, 4, 3), "60 minutes or 1 hour"))
      raw_data$crf_means_score <- get_binary_score(raw_data$crf_means, c(2, "How well the heart can pump blood and the lungs can provide oxygen"))
      raw_data$ms_means_score <- get_binary_score(raw_data$ms_means, c(1, "How well the muscles can push, pull or stretch"))
      raw_data$sports_skill_score <- get_binary_score(raw_data$sports_skill, c(4, "Watch a video, take a lesson or have a coach teach you how to kick and catch"))
      raw_data$fill_in_the_blanks_score <- get_fill_in_the_blanks_score(raw_data$pa_is, raw_data$pa_is_also, raw_data$improve, raw_data$increase, raw_data$when_cooling_down, raw_data$heart_rate, version)
      raw_data$ku_score <- get_ku_score(raw_data$pa_guideline_score, raw_data$crf_means_score, raw_data$ms_means_score, raw_data$sports_skill_score, raw_data$fill_in_the_blanks_score)
      raw_data$ku_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$ku_score, "ku")
      raw_data$ku_status <- get_capl_domain_status(raw_data, "ku")

      # Overall physical literacy computations
      raw_data$capl_score <- get_capl_score(raw_data$pc_score, raw_data$db_score, raw_data$mc_score, raw_data$ku_score)
      raw_data$capl_interpretation <- get_capl_interpretation(raw_data$age, raw_data$gender, raw_data$capl_score, "capl")
      raw_data$capl_status <- get_capl_domain_status(raw_data, "capl")

      # Sorting
      if(is.na(sort) | is.null(sort) | sort == "" | length(sort) == 0 | sort == "asis") {
        # Don't sort variables in raw_data
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
#' @description
#' This function computes the status ("complete", "missing interpretation", "missing protocol" or "incomplete") of a CAPL domain (e.g., `pc_status`,
#' `db_status`, `mc_status`, `ku_status`, `capl_status`).
#'
#' @export
#'
#' @param x A data frame that includes the required variables for a given domain (see Details).
#' @param domain A character vector representing one of the CAPL-2 domains (valid values are "pc", "db", "mc", "ku" and "capl")
#'
#' @details
#' If the `domain` argument is set to "pc", the following variables must be included in the `x` argument:
#' * `pc_score`
#' * `pc_interpretation`
#' * `pacer_score`
#' * `plank_score`
#' * `camsa_score`
#'
#' If the `domain` argument is set to "db", the following variables must be included the `x` argument:
#' * `db_score`
#' * `db_interpretation`
#' * `step_score`
#' * `self_report_pa_score`
#'
#' If the `domain` argument is set to "mc", the following variables must be included the `x` argument:
#' * `mc_score`
#' * `mc_interpretation`
#' * `predilection_score`
#' * `adequacy_score`
#' * `intrinsic_motivation_score`
#' * `pa_competence_score`
#'
#' If the `domain` argument is set to "ku", the following variables must be included the `x` argument:
#' * `ku_score`
#' * `ku_interpretation`
#' * `pa_guideline_score`
#' * `crf_means_score`
#' * `ms_means_score`
#' * `sports_skill_score`
#' * `fill_in_the_blanks_score`
#'
#' If the `domain` argument is set to "capl", the following variables must be included the `x` argument:
#' * `capl_score`
#' * `capl_interpretation`
#' * `pc_score`
#' * `db_score`
#' * `mc_score`
#' * `ku_score`
#' * `capl_score`
#'
#' Other `capl` functions called by this function include: [validate_character()] and [validate_number()].
#'
#' @examples
#' capl_demo_data <- get_capl_demo_data(3)
#'
#' capl_results <- get_capl(capl_demo_data)
#'
#' get_capl_domain_status(capl_results, "pc")
#'
#' # [1] "complete"               "incomplete"             "missing interpretation"
#'
#' @return Returns a character vector with a value of "complete", "missing interpretation", "missing protocol" or "incomplete".
get_capl_domain_status <- function(x = NULL, domain = NA) {
  domain <- validate_character(domain[1])
  try(
    if(is.null(x)) {
      stop("[CAPL error]: the x argument is missing.")
    } else if(! isTRUE("data.frame" %in% class(x))) {
      stop("[CAPL error]: the x argument must be a data frame.")
    } else if(is.na(domain)) {
      stop("[CAPL error]: the domain argument is missing.")
    } else if(! domain %in% c("pc", "db", "mc", "ku", "capl")) {
      stop("[CAPL error]: the domain argument value is not valid (valid values are 'pc', 'db', 'mc', 'ku' or 'capl').")
    } else {
      if(domain == "pc") {
        required_variables <- c("pc_score", "pc_interpretation", "pacer_score", "plank_score", "camsa_score")
      } else if(domain == "db") {
        required_variables <- c("db_score", "db_interpretation", "step_score", "self_report_pa_score")
      } else if(domain == "mc") {
        required_variables <- c("mc_score", "mc_interpretation", "predilection_score", "adequacy_score", "intrinsic_motivation_score", "pa_competence_score")
      } else if(domain == "ku") {
        required_variables <- c("ku_score", "ku_interpretation", "pa_guideline_score", "crf_means_score", "ms_means_score", "sports_skill_score", "fill_in_the_blanks_score")
      } else if(domain == "capl") {
        required_variables <- c("capl_score", "capl_interpretation", "pc_score", "db_score", "mc_score", "ku_score")
      } else {
        required_variables <- NA
      }
      if(sum(required_variables %in% colnames(x)) < length(required_variables)) {
        stop("[CAPL error]: x is missing some of the required variables that are necessary to compute the domain status.")
      } else {
        x <- x[required_variables]
        x[, 2] <- sapply(x[, 2], validate_character)
        x[-2] <- lapply(x[-2], validate_number)
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
    }
  )
}

#' Compute a CAPL-2 interpretation for a given CAPL-2 protocol or domain score.
#'
#' @description
#' This function computes an age- and gender-specific CAPL-2 interpretation for a given CAPL-2 protocol or domain score (e.g., `pc_interpretation`).
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param age A numeric vector (valid values are between 8 and 12).
#' @param gender A character vector (valid values currently include "girl", "g", "female", "f", "boy", "b", "male", "m").
#' @param score A numeric vector. If the `protocol` argument is set to "pacer" or "steps", this argument must contain integers.
#' @param protocol A character vector representing a CAPL protocol (valid values include "pacer", "plank", "camsa", "pc", "steps",
#' "self_report_pa", "db", "mc", "ku", "capl"; valid values are not case-sensitive).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_age()], [validate_gender()], [validate_character()], [validate_number()] and
#' [validate_scale()]. This function will check whether a score for a given protocol is within a valid range; if not, NA will be returned.
#'
#' @examples
#' get_capl_interpretation(
#'   age = 7:13,
#'   gender = c("g", "g", "b", "Boy", "m", "f", "Female"),
#'   score = c(50, 25, 100, 5, 150, 23, 78),
#'   protocol = "pacer"
#' )
#'
#' # [1] NA            "achieving"   "excelling"   "beginning"   "excelling"   "progressing"
#' # [7] NA
#'
#' @return Returns a character vector with values of "beginning", "progressing", "achieving" or "excelling" (if valid) or NA (if not valid).
get_capl_interpretation <- function(age = NA, gender = NA, score = NA, protocol = NA) {
  try(
    if(var(c(length(age), length(gender), length(score))) == 0) {
      pacer_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(10, 25, 37, 37, 9, 19, 27, 27, 10, 27, 39, 39, 10, 21, 29, 29, 11, 28, 41, 41, 10, 21, 30, 30, 11, 30, 43, 43, 11, 23, 32, 32, 13, 33, 48, 48, 12, 26, 36, 36),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      plank_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(12.4, 72.0, 101.0, 101.0, 24.4, 59.4, 89.3, 89.3, 15.2, 74.9, 103.8, 103.8, 25.2, 61.4, 92.2, 92.2, 18.1, 77.7, 106.7, 106.7, 26.0, 63.4, 95.2, 95.2, 20.9, 80.6, 109.5, 109.5, 26.8, 65.3, 98.2, 98.2, 23.8, 83.4, 112.4, 112.4, 27.6, 67.3, 101.2, 101.2),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      camsa_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(16, 21, 23, 23, 15, 20, 21, 21, 17, 22, 23, 23, 16, 21, 22, 22, 17, 22, 24, 24, 17, 22, 23, 23, 18, 23, 25, 25, 17, 22, 24, 24, 18, 24, 26, 26, 18, 23, 25, 25),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      pc_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(13.4, 19.4, 22.0, 22.0, 13.2, 18.0, 20.3, 20.3, 13.7, 19.9, 22.5, 22.5, 13.7, 18.6, 20.9, 20.9, 14.0, 20.3, 23.0, 23.0, 14.1, 19.1, 21.6, 21.6, 14.3, 20.8, 23.6, 23.6, 14.5, 19.8, 22.3, 22.3, 14.9, 21.6, 24.5, 24.5, 15.2, 20.7, 23.3, 23.3),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      steps_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(8892, 11999, 17980, 17980, 8059, 11999, 15643, 15643, 8655, 11999, 17500, 17500, 7814, 11999, 15168, 15168, 8417, 11999, 17020, 17020, 7569, 11999, 14692, 14692, 8180, 11999, 16539, 16539, 7324, 11999, 14217, 14217, 7942, 11999, 16059, 16059, 7079, 11999, 13742, 13742),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      self_report_pa_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(4, 4, 6, 6, 4, 4, 6, 6, 4, 4, 6, 6, 3, 4, 6, 6, 4, 4, 6, 6, 3, 4, 6, 6, 4, 4, 6, 6, 3, 4, 6, 6, 4, 4, 6, 6, 3, 4, 6, 6),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      db_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(8.8, 22.3, 26.9, 26.9, 10.8, 21.6, 26.2, 26.2, 8.8, 22.3, 26.9, 26.9, 10.7, 21.5, 26.1, 26.1, 8.8, 22.3, 26.9, 26.9, 10.5, 21.1, 25.7, 25.7, 8.8, 22.3, 26.9, 26.9, 10.1, 20.4, 24.8, 24.8, 8.8, 22.3, 26.9, 26.9, 10.1, 20.3, 24.7, 24.7),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      mc_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(16.3, 23.0, 25.3, 25.3, 16.2, 22.3, 24.8, 24.8, 16.7, 23.3, 25.7, 25.7, 16.2, 22.3, 24.8, 24.8, 16.8, 23.5, 26.0, 26.0, 16.2, 22.3, 24.8, 24.8, 16.8, 23.7, 26.0, 26.0, 16.2, 22.5, 25.0, 25.0, 16.8, 23.7, 26.2, 26.2, 16.3, 22.5, 25.0, 25.0),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      ku_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(4.4, 6.4, 7.2, 7.2, 4.8, 6.6, 7.3, 7.3, 4.7, 6.8, 7.6, 7.6, 5.0, 6.9, 7.7, 7.7, 5.0, 7.2, 8.1, 8.1, 5.3, 7.3, 8.1, 8.1, 5.2, 7.5, 8.4, 8.4, 5.5, 7.6, 8.4, 8.4, 5.3, 7.6, 8.5, 8.5, 5.6, 7.8, 8.6, 8.6),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      capl_lookup <- data.frame(
        age = c(rep(8, 8), rep(9, 8), rep(10, 8), rep(11, 8), rep(12, 8)),
        gender = c(rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4), rep("boy", 4), rep("girl", 4)),
        bound = c(47.3, 65.3, 72.7, 72.7, 49.6, 64.8, 71.7, 71.7, 48.8, 67.4, 75.0, 75.0, 50.6, 66.1, 73.1, 73.1, 49.8, 68.7, 76.4, 76.4, 51.2, 66.8, 73.9, 73.9, 50.2, 69.3, 77.1, 77.1, 51.3, 67.0, 74.1, 74.1, 51.6, 71.1, 79.1, 79.1, 52.1, 68.1, 75.3, 75.3),
        interpretation = rep(c("beginning", "progressing", "achieving", "excelling"), 10)
      )
      empty_lookup <- data.frame(age = NA, gender = NA, bound = NA, interpretation = NA)
	  age <- validate_age(age)
      gender <- validate_gender(gender)
      score <- validate_number(score)
	  protocol <- validate_character(protocol)
      if(protocol == "camsa") {
		score <- score * 2.8
	  }
      return(
        unname(
          apply(data.frame(age, gender, score, rep(tolower(protocol[1]), length(age))), 1, function(x) {
            age <- validate_age(x[1])
            gender <- validate_gender(x[2])
            score <- validate_number(x[3])
			protocol <- as.character(x[4])
            if(! is.na(score)) {
              if(protocol == "pacer") {
                score <- validate_scale(score, 1, 229)
              } else if(protocol == "plank") {
                score <- ifelse(score < 0, NA, score)
              } else if(protocol == "camsa") {
				score <- suppressWarnings(as.numeric(score))
                if(is.na(score) | score %% 1 > 0.000001 | score < 1 | score > 28) {
				  score <- NA
				} else {
				  score <- score
				}
              } else if(protocol == "steps") {
                score <- validate_scale(score, 1000, 30000)
              } else if(protocol == "self_report_pa") {
                score <- validate_scale(score, 0, 7)
              } else if(protocol == "db") {
                score <- validate_scale(score, 0, 30)
              } else {
                # Do nothing
              }
            }
            if(sum(is.na(c(age, gender, score, protocol))) > 0 | ! protocol %in% c("pacer", "plank", "camsa", "pc", "steps", "self_report_pa", "db", "mc", "ku", "capl")) {
              return(NA)
            } else {
              if(protocol == "pacer") {
                lookup <- pacer_lookup
              } else if(protocol == "plank") {
                lookup <- plank_lookup
              } else if(protocol == "camsa") {
                lookup <- camsa_lookup
              } else if(protocol == "pc") {
                lookup <- pc_lookup
              } else if(protocol == "steps") {
                lookup <- steps_lookup
              } else if(protocol == "self_report_pa") {
                lookup <- self_report_pa_lookup
              } else if(protocol == "db") {
                lookup <- db_lookup
              } else if(protocol == "mc") {
                lookup <- mc_lookup
              } else if(protocol == "ku") {
                lookup <- ku_lookup
              } else if(protocol == "capl") {
                lookup <- capl_lookup
              } else {
                lookup <- empty_lookup
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

#' Compute an overall physical literacy score.
#'
#' @description
#' This function computes an overall physical literacy score (`capl_score`) based on the physical competence (`pc_score`), daily behaviour (`db_score`),
#' motivation and confidence (`mc_score`), and knowledge and understanding (`ku_score`) domain scores. If one of the scores is missing or invalid, a
#' weighted score will be computed from the other three scores.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param pc_score A numeric vector (valid values are between 0 and 30).
#' @param db_score A numeric (integer) vector (valid values are between 0 and 30).
#' @param mc_score A numeric vector (valid values are between 0 and 30).
#' @param ku_score A numeric vector (valid values are between 0 and 10).
#'
#' @details
#' Other `capl` functions called by this function include: [validate_number()], [validate_integer()] and [validate_domain_score()].
#'
#' @examples
#' get_capl_score(
#'   pc_score = c(20, 15, 12, 5, 31),
#'   db_score = c(20, 15, 6, 4.1, 25),
#'   mc_score = c(20, 20, 19, 15.4, 25),
#'   ku_score = c(11, 4, 5, 7.8, 10)
#' )
#'
#' # [1] 66.66667 54.00000 42.00000 40.28571 85.71429
#'
#' @return Returns a numeric vector with values between 0 and 100 (if valid) or NA (if not valid).
get_capl_score <- function(pc_score = NA, db_score = NA, mc_score = NA, ku_score = NA) {
  try(
    if(var(c(length(pc_score), length(db_score), length(mc_score), length(ku_score))) == 0) {
      return(
        unname(
          apply(data.frame(pc_score, db_score, mc_score, ku_score), 1, function(x) {
            pc_score <- validate_domain_score(x[1], "pc")
            db_score <- validate_domain_score(x[2], "db")
            mc_score <- validate_domain_score(x[3], "mc")
            ku_score <- validate_domain_score(x[4], "ku")
            if(sum(is.na(c(pc_score, db_score, mc_score, ku_score))) > 1) {
              return(NA)
            } else if(sum(is.na(c(pc_score, db_score, mc_score, ku_score))) == 1) {
              if(is.na(ku_score)) {
                denominator <- 90
              } else {
                denominator <- 70
              }
              return(sum(pc_score, db_score, mc_score, ku_score, na.rm = TRUE) * 100 / denominator)
            } else {
              return(sum(pc_score, db_score, mc_score, ku_score))
            }
          })
        )
      )
    } else {
      stop("[CAPL error]: the pc_score, db_score, mc_score and ku_score arguments must be the same length.")
    }
  )
}
