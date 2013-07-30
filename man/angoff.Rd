\name{angoff}
\alias{angoff}
\title{Compute Angoff Coefficient}
\usage{
  angoff(x, split.method = "even.odd",
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
  \item{angoff}{The estimate of reliability.}
  \item{Split}{The split half key used to calculate
  angoff's coefficient.}
}
\description{
  Angoff's coefficient is most appropriately used for
  estimating reliability in tests that can be split into
  two parts with unequal lengths.  The calculation corrects
  for the inequality of length in the splits.  Angoff's
  coefficient is also believed to handle congeneric test
  structures relatively well.
}
\examples{
angoff(Rosenberg, split.method="even.odd", missing="complete", standardize=FALSE)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Feldt, L. S., & Charter, R. A. (2003). Estimating the
  reliability of a test split into two parts of equal or
  unequal length. Psychological Methods, 8(1), 102-109.

  Sedere, M. U. And Feldt, L. S. (1977), The Sampling
  Distributions Of The Kristof Reliability Coefficient, The
  Feldt Coefficient, And Guttman's Lambda-2. Journal Of
  Educational Measurement, 14: 53-62.

  Feldt, L. S. (1975). Estimation of the reliability of a
  test divided into two parts of unequal length.
  Psychometrika, 40, 557-561.

  Angoff, W. H. (1953). Test reliability and effective test
  length. Psychometrika, 18, 1-14.
}

