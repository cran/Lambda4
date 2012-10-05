\name{lambda3}
\alias{lambda3}
\title{Guttmans Lambda3}
\description{Often recognized as Cronbachs alpha, Guttman's lambda3 can be used to estimate reliability when the data can be split in parallel forms.
}
\usage{
lambda3(x, item.stats.max = 12)
}
\arguments{
  \item{x}{data in the form of a covariance matrix or observed data frame.}
  \item{item.stats.max}{The max number of items to display the item statistics.}
}
\value{
	\item{lambda3}{Includes the estimates of lambda3}
\item{Unstandardized}{Unstandaridized estimate if the input matrix was observed data or a covariance matrix}
\item{Standardized}{lambda3 estimator performed on a correlation matrix.}
\item{Items}{Number of items}
}
\references{
Cronbach L (1951). "Coefficient Alpha and the Internal Structure of Tests." Psychometrika, 16, 297-334. \\
Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282. \\
}
\author{
Tyler Hunt
}

\examples{
	data(Rosenberg)
	lambda3(Rosenberg)}

\keyword{lambda3}
\keyword{cronbachs alpha}
