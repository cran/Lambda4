\name{lambda1}
\alias{lambda1}
\title{Compute Guttman's Lambda 1 Coefficient}
\usage{
  lambda1(x, missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{an object that you can compute the covariance
  of}

  \item{missing}{how to handle missing values.}

  \item{standardize}{Results are standardized by using the
  correlation matrix instead of the covariance matrix for
  computation.}
}
\description{
  Compute Guttman's Lambda 1 Coefficient
}
\examples{
lambda1(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

