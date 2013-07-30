\name{quant.lambda4}
\alias{quant.lambda4}
\title{Compute Quantile Lambda 4}
\usage{
  quant.lambda4(x, starts = 1000, quantiles = 0.5,
    missing = "complete", show.lambda4s = FALSE,
    standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{starts}{How many split-half reliability estimates
  used}

  \item{quantiles}{The quantiles of the generated splits.
  It defaults to .5 because it makes the most sense at this
  time.  (The simulation manuscript is under review).}

  \item{missing}{How to handle missing values.}

  \item{show.lambda4s}{If TRUE then Shows the vector of
  lambda4s if FALSE then the vector is hidden}

  \item{standardize}{Results are standardized by using the
  correlation matrix instead of the covariance matrix for
  computation.}
}
\value{
  \item{lambda4.quantile}{The user specified quantile value
  of the vector of maximized split-reliability}
  \item{lambda4.optimal}{Maximum split-half reliability
  (Maximized Lambda4} \item{l4.vect}{A vector of lambda4
  (split-half reliability) calculations}
}
\description{
  Quantile maximize lambda4 is a statistic that can be used
  in most measurement situations.  In particular this
  function generates a vector t of length equal to the
  number of items.  Each value in the vector consists of
  either a +1 or -1 (randomly generated).  Next, in a
  random order each value in the t-vector is switched.  The
  value kept (+1 or -1) is the value that resulted in the
  highest reliability estimate.  This procedure is repeated
  by default 1000 times but can also be user specified.
  The user can then specify the quantile of this vector but
  it defaults to .5.
}
\examples{
quant.lambda4(Rosenberg, starts=1000, quantile=c(.05,.5,.95))
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.

  Callender J, Osburn H (1977). "A Method for Maximizing
  and Cross-Validating Split-Half Reliability
  Coefficients." Educational and Psychological Measurement,
  37, 819-826.

  Callender J, Osburn H (1979). "An Empirical Comparison of
  Coefficient Alpha, Guttman's Lambda2 and Msplit Maximized
  Split-Half Reliability Estimates." Journal of Educational
  Measurement, 16, 89-99. Guttman L (1945). "A Basis for
  Analyzing Test-Retest Reliability." Psychometrika, 10,
  255-282.

  Callender J, Osburn H (1977). "A Method for Maximizing
  and Cross-Validating Split-Half Reliability
  Coefficients." Educational and Psychological Measurement,
  37, 819-826.

  Callender J, Osburn H (1979). "An Empirical Comparison of
  Coefficient Alpha, Guttman's Lambda2 and Msplit Maximized
  Split-Half Reliability Estimates." Journal of Educational
  Measurement, 16, 89-99.

  Sijtsma K (2009). "On the Use, Misuse, and Very Limited
  Usefulness of Cronbach's Alpha." Psychometrika, 74(1),
  107-120.
}

