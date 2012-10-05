\name{user.lambda4}
\alias{user.lambda4}
\title{User Specified Lambda4}
\description{
This function gets back to the roots of Guttman's (1945) conception of reliability estimation.  Specifically, it allows users to specify the split of items that they want for calculating Guttman's lambda4.  Additionally, it allows a random split or an evenly random split.
}
\usage{user.lambda4(x, split.method = "even.odd", bootstrap = FALSE, B = 1000, show.boots = FALSE, item.stats.max = 12)}
\arguments{
  \item{x}{This is data that can be a covariance matrix or a normal data frame.}
  
  \item{split.method}{Three options for splits.  Also, one could indicate the split desired.
  	"odd.even" All odd items on one split and even items on the other.
  	"random" randomly splits the items.
  	"evenly.random" randomly splits the items but forces each split to have an equivalent number of items.}

  \item{bootstrap}{If bootstrap=TRUE then confidence intervals are calculated for the indicated split of items.}
  \item{B}{The number of bootstraps.}
  \item{show.boots}{Shows lambda4 estimates for each bootstrapped sample.}
  \item{item.stats.max}{Item stats are suppressed if the number of items is larger than this value.}
}
\details{
This 
}
\value{
\item{lambda4}{Includes the estimate of lambda4}
	\item{LowerCI}{Lower confidence interval obtained if bootstrap=TRUE.}
	\item{Estimate}{The lambda4 estimate based on the specified split.}
	\item{UpperCI}{Upper confidence interval obtained if bootstrap=TRUE.}
\item{Item.Statistics}{Includes the stats for each item}
	\item{Split}{Indicates the way the items were split.}
	\item{Mean}{Mean of the items.}
	\item{SD}{Standard deviation of items.}
	\item{Obs}{Number of observations.}
\item{Boots}{The lambda4 values for each bootstrapped sample.}
}
\references{Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282.}
\author{Tyler Hunt}

\examples{
	data(Guttman)
	user.lambda4(Guttman, split.method="evenly.random")}
\keyword{Split-Half}
\keyword{Reliability}