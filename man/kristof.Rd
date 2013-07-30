\name{kristof}
\alias{kristof}
\title{Compute Kristof Coefficient}
\usage{
  kristof(x, split.method = "triplet",
    missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{split.method}{Specify method for splitting items?}

  \item{missing}{How to handle missing values.}

  \item{standardize}{When TRUE Results are standardized by
  using the correlation matrix instead of the covariance
  matrix for computation.}
}
\value{
  \item{kristof}{The Kristof estimate of reliability.}
  \item{Split}{The split used to obtain the reliability
  estimate.}
}
\description{
  A reliability coefficient used for tests that are easily
  split into three parts.
}
\examples{
kristof(Rosenberg, split.method="triplet")
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Kristof, W. (1974). Estimation of reliability and true
  score variance from a split of a test into three
  arbitrary parts. Psychometrika, 39(4), 491-499.
}

