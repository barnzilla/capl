% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physical_competence.R
\name{get_camsa_score}
\alias{get_camsa_score}
\title{Select the maximum CAMSA skill + time score.}
\usage{
get_camsa_score(camsa_skill_time_score1 = NA, camsa_skill_time_score2 = NA)
}
\arguments{
\item{camsa_skill_time_score1}{A numeric (integer) vector representing the skill + time score from trial 1 (valid values are between 1 and 28).}

\item{camsa_skill_time_score2}{A numeric (integer) vector representing the skill + time score from trial 2 (valid values are between 1 and 28).}
}
\value{
Returns a numeric vector with values between 0 and 10 (if valid) or NA (if not valid).
}
\description{
This function selects the maximum CAMSA (Canadian Agility and Movement Skill Assessment) skill + time score for two trials (\code{camsa_score})
and then divides by 2.8 so that the score is out of 10. This score is used to compute the physical literacy score (\code{pc_score}).
}
\details{
Other \code{capl} functions called by this function include: \code{\link[=validate_scale]{validate_scale()}}.
}
\examples{
get_camsa_score(
  camsa_skill_time_score1 = c(1, 5, 10, 28, 29), 
  camsa_skill_time_score2 = c(5, 7, 12, NA, 27)
)

# [1]  5  7 12 NA NA

}
