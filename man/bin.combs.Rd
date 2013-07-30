\name{bin.combs}
\alias{bin.combs}
\title{Generate Unique Binary Combinations}
\usage{
  bin.combs(p)
}
\arguments{
  \item{p}{The number of items in the test.}
}
\value{
  \item{}{Function returns a matrix of binary combinations
  coded as either -1 or 1.}
}
\description{
  Provides all of the unique binary combinations for the
  cov.lambda4 function.  It should be noted that this
  function does not provide all combinations but only ones
  that are unique for the cov.lambda4 function.  That is a
  vector coded c(0,1,0,1) is equivalent to a vector
  c(1,0,1,0) and only one of them is generated.
}
\examples{
bin.combs(4)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}

