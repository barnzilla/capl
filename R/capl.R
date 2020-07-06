#' Compute an age- and gender-specific CAPL interpretation for a given CAPL protocol or domain score.
#'
#' @export
#'
#' @importFrom stats var
#'
#' @param age a numeric element or vector (valid values are between 8 and 12).
#' @param gender a character element or vector (valid values currently include "girl", "g", "female", "f", "boy", "b", "male", "m").
#' @param score a numeric element or vector.
#' @param protocol a character element representing a CAPL protocol (valid values currently include "pacer", "plank", "camsa").
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
            if(sum(is.na(c(age, gender, score, protocol))) > 0 | ! protocol %in% c("pacer", "plank", "camsa")) {
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