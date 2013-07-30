\name{lambda5}
\alias{lambda5}
\title{Compute Guttman's Lambda 5 Coefficient}
\usage{
  lambda5(x, missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix.}

  \item{missing}{how to handle missing values.}

  \item{standardize}{Results are standardized by using the
  correlation matrix instead of the covariance matrix for
  computation.}
}
\description{
  Compute Guttman's Lambda 5 Coefficient
}
\examples{
lambda5(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

