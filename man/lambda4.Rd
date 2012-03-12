\name{lambda4}
\alias{lambda4}
\title{Lambda 4}
\description{This code creates a vector of semi-maximized split half reliabilities and then computes a maximum, minimum, median, and mean of this vector.
}
\usage{
lambda4(x)
}
\arguments{
  \item{x}{This code does not compute on covariance matrices.  Participant data will be required for computation.

}
}
\details{The maximum of this list is the most similar to other maximized reliability coefficients (e.g. glb). In this calculation context it is obvious why a bias is present for small samples. The standard error of the coefficient is high therefore allowing greater deviations from the parameter. By taking a maximum we capitalize on the greatest of these deviations thus biasing the statistic. Also, two other steps in the process may add a small amount of biasing error. First, by allowing the two splits to differ in their total number of items, biasing error due to flexibility is included. This is avoided entirely in the current calculation method but not in other popular maximized reliability coefficients. Second, when splitting the items up based on consecutively maximized covariances it is possible to capture biasing error. Instead of using the maximum statistic, the minimum or mean may be more appropriate. The mean may be lessed biased because it does not capitalize on the random deviations. The minimum may pose as a conservative estimate that rarely overestimes reliability. Specifically, the minimum would capitalize on biasing error in the lower direction. Thus counteracting the potential bias from the covariance splitting procedure.

}
\value{
\item{lambda4.list}{This is a list of all the lambda4s computed after splitting based on maximized inter-item covariances}
\item{max.lambda4}{Maximum value returned in the lambda4.list}
\item{mean.lambda4}{The mean of the values returned in the lambda4.list}
\item{min.lambda4}{Minimum value returned in the lambda4.list}
\item{median.lambda4}{Median value returned in the lambda4.list}
}
\references{
Callender, J. and Osburn, H. (1977). A method for maximizing and cross validating split half reliability coefficients. Educational and Psychological Measurement, 37:819-826.


Callender, J. and Osburn, H. (1979). An empirical comparison of coefficient alpha, Guttman's lambda2 and msplit maximized split-half reliability estimates. Journal of Educational Measurement, 16:89-99.

Guttman, L. (1945). A basis for analyzing test-retest reliability. Psychometrika, 10:255-282.

Osburn, H. (2000). Coefficient alpha and related internal consistency reliability coefficients. Psychological Methods, 5:343-355.

}
\author{
Tyler Hunt
}
\note{Simulations are currently underway in testing if this method is less biased than similar statistics.
}
\seealso{Sijtsma, K. (2009). On the use, misuse, and very limited usefulness of Cronbach's alpha. Psychometrika, 74(1):107-120.

}
\examples{lambda4(USJudgeRatings)
}
\keyword{Reliability}