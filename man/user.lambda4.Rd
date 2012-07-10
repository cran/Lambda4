\name{user.lambda4}
\alias{user.lambda4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{User Specified Split-Half Reliability
}
\description{
This function enables users to select the splits that items are on.  Thereby giving complete control
}
\usage{
user.lambda4(x, split = "even odd")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data matrix or covariance matrix.
}
  \item{split}{
defaults to an odd even split but user can specify there own splits with 0s and 1s (example below). 
}
}
\value{
\item{Estimate}{Split half reliability estimate of the specified split.}
}
\references{
Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282.
}
\author{
Tyler Hunt
}
\examples{
# The function defaults to an odd even split
user.lambda4(Harman74.cor$cov)

# You can specify a Split
user.lambda4(Harman74.cor$cov, c(0,0,1,1,0,0,1,1,0,1,0,1,0,0,1,1,1,0,0,0,1,1,0,1))

# You can select a random split
user.lambda4(Harman74.cor$cov, round(runif(24)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Split-Half }
\keyword{ Reliability }% __ONLY ONE__ keyword per line
