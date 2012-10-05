\name{cov.lambda4}
\alias{cov.lambda4}

\title{Covariance Maximized Lambda 4}
\description{This code estimates Maximized lambda4 a
split half reliability estimate that splits the halves by specifying a two 
column list of paired inter-item covariances in descending order.  It then
calculates Lambda4 on every possible split half while preserving the inter-item pairings.  The function then returns a list of the Lambda4s and then takes the minimum, maximum, median, and mean of the list.}
\usage{cov.lambda4(x, show.lambda4s = FALSE)}

\arguments{
\item{x}{Can be either a data matrix or a covariance matrix.}
\item{show.lambda4s}{If TRUE then Shows the vector of lambda4s if FALSE then the vector is hidden}
}

\value{
\item{l4.vect}{This is a list of all the lambda4s computed after splitting based on maximized inter-item covariances}
\item{lambda4}{Includes various summary statistics of l4.vect}
\item{Max}{Maximum value of l4.vect}
\item{Mean}{The mean of the values of l4.vect}
\item{Min}{Minimum value of l4.vect}
\item{Median}{Median value of l4.vect}
\item{Analysis.Details}{Includes description of the number of splits}
\item{Items}{The number of items}
\item{lambda4s}{The number of unique splits(after maximization)}
}
\references{
Cronbach L (1951). "Coefficient Alpha and the Internal Structure of Tests." Psychometrika, 16, 297-334.
 
Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282.

Callender J, Osburn H (1977). "A Method for Maximizing and Cross-Validating Split-Half Reliability Coefficients." Educational and Psychological Measurement, 37, 819-826.

Callender J, Osburn H (1979). "An Empirical Comparison of Coefficient Alpha, Guttman's Lambda2 and Msplit Maximized Split-Half Reliability Estimates." Journal of Educational Measurement, 16, 89-99.

}
\author{Tyler Hunt}
\note{This code has a lower level bias in small samples < 80 than the quant.lambda4 function.}

\seealso{Sijtsma K (2009). "On the Use, Misuse, and Very Limited Usefulness of Cronbach's Alpha." Psychometrika, 74(1), 107-120.}
\examples{
	data(Rosenberg)
	cov.lambda4(Rosenberg)}
\keyword{Reliability}
\keyword{lambda4}