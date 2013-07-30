\name{impute.cov}
\alias{impute.cov}
\title{Compute Covariance Matrix}
\usage{
  impute.cov(x, missing = c("complete", "pairwise", "mi"))
}
\arguments{
  \item{x}{A data matrix}

  \item{missing}{how to handle missing values.}
}
\description{
  Implements various missing data techniques and generates
  a covariance matrix.
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

