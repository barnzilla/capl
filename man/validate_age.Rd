% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{validate_age}
\alias{validate_age}
\title{Check whether an age is valid for CAPL-2.}
\usage{
validate_age(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
Returns a numeric (integer) vector with a value between 8 and 12 (if valid) or NA (if not valid).
}
\description{
This function checks whether an age is valid (numeric and between 8 and 12). CAPL-2 scores and interpretations are valid for children between the ages
of 8 and 12 years.
}
\details{
If \code{x} contains a decimal value that is otherwise valid (e.g., 8.5, 10.1), this function will return the \code{\link[=floor]{floor()}} of the value.

Other \code{capl} functions called by this function include: \code{\link[=validate_number]{validate_number()}}.
}
\examples{
validate_age(c(7:13, "", NA, "12", 8.5))

# [1] NA  8  9 10 11 12 NA NA NA 12  8

}
