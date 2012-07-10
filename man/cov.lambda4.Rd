\name{cov.lambda4}
\alias{cov.lambda4}
\title{Covariance Maximized Lambda 4}
\description{This code estimates Maximized lambda4 a
split half reliability estimate that splits the halves by specifying a two 
column list of paired inter-item covariances in descending order.  It then
calculates Lambda4 on every possible split half while preserving the inter-item
pairings.  The function then returns a list of the Lambda4s and then takes the
minimum, maximum, median, and mean of the list.
}
\usage{
cov.lambda4(x, show.splits = FALSE)
}
\arguments{
\item{x}{Can be either a data matrix or a covariance matrix.
}
\item{show.splits}{If TRUE then Shows the vector of lambda4s if FALSE then the vector is hidden}
}

\value{
\item{l4.vect}{This is a list of all the lambda4s computed after splitting based on maximized inter-item covariances}
\item{max.lambda4}{Maximum value returned in the lambda4.list}
\item{mean.lambda4}{The mean of the values returned in the lambda4.list}
\item{min.lambda4}{Minimum value returned in the lambda4.list}
\item{median.lambda4}{Median value returned in the lambda4.list}
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
	This code has a lower level bias in small samples < 80 than the quantile lambda4 code.
}
\seealso{Sijtsma K (2009). "On the Use, Misuse, and Very Limited Usefulness of Cronbach's Alpha." Psychometrika, 74(1), 107-120.}
\examples{cov.lambda4(USJudgeRatings)
}
\keyword{Reliability}
