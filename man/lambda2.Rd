\name{lambda2}
\alias{lambda2}
\title{Compute Guttman's Lambda 2 Coefficient}
\usage{
  lambda2(x, missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{missing}{how to handle missing values.}

  \item{standardize}{Results are standardized by using the
  correlation matrix instead of the covariance matrix for
  computation.}
}
\description{
  Compute Guttman's Lambda 2 Coefficient
}
\examples{
lambda2(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

