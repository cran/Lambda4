\name{bin.combs}
\alias{bin.combs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Unique Binary Combinations}
\description{
Function provides all of the unique binary combinations for the cov.lambda4 function.  It should be noted that this function does not provide all combinations but only ones that are unique for the cov.lambda4 function.  That is a vector coded c(0,1,0,1) is equivalent to a vector c(1,0,1,0) and only one of them is computed.}
\usage{
bin.combs(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{The number of items
}
}
\value{Function returns a matrix of binary combinations coded as either -1 or 1.}

\author{
Tyler Hunt
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Bincombinations functin in e1071 \\
}
\examples{
bin.combs(4)
}
\keyword{Combinations}
