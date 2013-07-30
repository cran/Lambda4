\name{cov.lambda4}
\alias{cov.lambda4}
\title{Compute Covariance Maximized Lambda4}
\usage{
  cov.lambda4(x, method = "Hunt", missing = "complete",
    show.lambda4s = FALSE, show.splits = FALSE,
    standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix.}

  \item{method}{Can specify either "Hunt" or "Osburn".}

  \item{missing}{How to handle missing values.}

  \item{show.lambda4s}{If TRUE then the estimates for each
  split are included in the output.}

  \item{show.splits}{If TRUE then a binary matrix is
  exported that describes the ways the items were split.}

  \item{standardize}{When TRUE results are standardized by
  using the correlation matrix instead of the covariance
  matrix for computation.}
}
\value{
  \item{estimates}{The mean, median, max, and min of the
  split-half reliabilities.} \item{lambda4s}{A vector of
  maximized split-half reliabilities.} \item{method}{The
  method chosen.  Either "Hunt" or "Osburn".}
  \item{Analysis.Details}{Returns the number of variables
  and the number of split-half reliabilities.}
  \item{Splits}{The binary indicators of the splits for the
  min, max, and median split-half reliability.}
  \item{show.splits}{Logical argument selected to show the
  splits.} \item{show.lambdas4s}{Logical argument selected
  to show the split-half reliabilities.}
}
\description{
  This code estimates maximized lambda4, a split-half
  reliability estimate.  The function splits the halves by
  specifying a two column list of paired inter-item
  covariances in descending order. It then calculates
  Guttman's lambda4 on every possible split-half while
  preserving the inter-item pairings. The function then
  returns a list of the Lambda4s and then takes the
  minimum, maximum, median, and mean of the list.  This
  calculation is most appropiately applied to tests with
  multiple factors.
}
\examples{
cov.lambda4(Rosenberg, method="Hunt")
cov.lambda4(Rosenberg, method="Osburn")
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

