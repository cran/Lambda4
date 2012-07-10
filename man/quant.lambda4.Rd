\name{quantile.lambda4}
\alias{quant.lambda4}
\title{Quantile Maximized Lambda4}
\description{Quantile maximize lambda4 is a statistic that can be used in most measurement
situations.  In particular this function generates a vector t of length equal to the 
number of items.  Each value in the vector consists of either a +1 or -1 
(randomly generated).  Next, in a random order each value in the t-vector is switched.  
The value kept (+1 or -1) is the value that resulted in the highest reliability estimate.  
This procedure is repeated by default 1000 times but can also be user specified.  
The user can then specify the quantile of this vector but it defaults to .5.
}
\usage{
quant.lambda4(x, starts = 1000, quantile = 0.5, show.splits=FALSE)
}
\arguments{
  \item{x}{
x is the data in the form of a correlation matrix, covariance matrix, or observed data.
}
  \item{starts}{
How many split-half reliabilities calculated
}
  \item{quantile}{
The quantile of the generated splits.  It defaults to .5 because it makes the most sense at this time.  (simulation manuscripts are in the works).
}
\item{show.splits}{If TRUE then Shows the vector of lambda4s if FALSE then the vector is hidden}
}
\value{
\item{lambda4.quantile}{The user specified quantile value of the vector of maximized split-reliability}
\item{lambda4.optimal}{Maximum split-half reliability (Maximized Lambda4}
\item{l4.vect}{A vector of lambda4 (split-half reliability) calculations}
}
\references{
Cronbach L (1951). "Coefficient Alpha and the Internal Structure of Tests." Psychometrika, 16, 297-334.
Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282.
Callender J, Osburn H (1977). "A Method for Maximizing and Cross-Validating Split-Half Reliability Coefficients." Educational and Psychological Measurement, 37, 819-826.
Callender J, Osburn H (1979). "An Empirical Comparison of Coefficient Alpha, Guttman's Lambda2 and Msplit Maximized Split-Half Reliability Estimates." Journal of Educational Measurement, 16, 89-99.
}
\author{
Tyler Hunt
}
\note{
This method has a lower level of bias than the covariance lambda4 calculation when sample size is > 80.
}
\seealso{
Sijtsma K (2009). "On the Use, Misuse, and Very Limited Usefulness of Cronbach's Alpha." Psychometrika, 74(1), 107-120.}
\examples{
quant.lambda4(USJudgeRatings, starts=1000,quantile=c(.05,.5,.95))
}
\keyword{Reliability}
\keyword{Maximized lambda4}