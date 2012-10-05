\name{bin.combs}
\alias{bin.combs}

\title{Unique Binary Combinations}

\description{
Function provides all of the unique binary combinations for the cov.lambda4 function.  It should be noted that this function does not provide all combinations but only ones that are unique for the cov.lambda4 function.  That is a vector coded c(0,1,0,1) is equivalent to a vector c(1,0,1,0) and only one of them is computed.}

\usage{bin.combs(p)}

\arguments{\item{p}{The number of items}}

\value{Function returns a matrix of binary combinations coded as either -1 or 1.}

\author{Tyler Hunt}

\seealso{bincombinations function in e1071}
\examples{bin.combs(4)}

\keyword{Combinations}