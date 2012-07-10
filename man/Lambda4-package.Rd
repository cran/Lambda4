\name{Lambda4-package}
\alias{Lambda4-package}
\alias{Lambda4}
\docType{package}
\title{New estimation techniques for the Maximized Lambda4 reliability estimate}
\description{These functions are the first to combine the logic of Cronbach's alpha and
maximized reliability coefficients.  The combination of these two previous methods allows 
researchers to calculate reliability of scales that do not meet alpha's stringent
assumptions and/or multidimensional scales while also minimizing the level of bias that 
has been pervasive in maximized reliability coefficients.
}
\details{
\tabular{ll}{
Package: \tab Lambda4\cr
Type: \tab Package\cr
Version: \tab 2.1\cr
Date: \tab 2012-09-20\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tyler Hunt
Maintainer: tyler.hunt@utah.edu
}
\references{
Cronbach L (1951). "Coefficient Alpha and the Internal Structure of Tests." Psychometrika, 16, 297-334. \\
Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282. \\
Callender J, Osburn H (1977). "A Method for Maximizing and Cross-Validating Split-Half Reliability Coefficients." Educational and Psychological Measurement, 37, 819-826. \\
Callender J, Osburn H (1979). "An Empirical Comparison of Coefficient Alpha, Guttman's Lambda2 and Msplit Maximized Split-Half Reliability Estimates." Journal of Educational Measurement, 16, 89-99. \\
}
\keyword{lambda4}
\keyword{reliability}
\keyword{greatest-lower-bound}
\keyword{maximal-reliability}
\seealso{
Sijtsma K (2009). "On the Use, Misuse, and Very Limited Usefulness of Cronbach's Alpha." Psychometrika, 74(1), 107-120.
}
\examples{
cov.lambda4(USJudgeRatings)
quant.lambda4(USJudgeRatings, starts=1000,quantile=c(.05,.5,.95))
}