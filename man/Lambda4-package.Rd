\name{Lambda4-package}
\alias{Lambda4-package}
\alias{Lambda4}
\docType{package}
\title{Estimation techniques for the reliability estimate Maximized Lambda4}
\description{This code estimates Maximized lambda4 a split half reliability estimate that splits the halves by specifying a two column list of paired interitem covariances in descending order.  Osburn (2000) recommended this as an estimation method for computing maximized lambda4 but only calculated Guttman's (1945) split half calculation (Lambda4) on one of the possible splits.
This code calculates split half reliability on every possible split half while preserving the inter item pairings.  The function then returns a list of the Lambda4s and then takes the
minimum, maximum, median, and mean of the list.  The mean of this list is most robust against random error.  The maximum of this list is the most similar to other maximized reliability coefficients (e.g. glb).  In this calculation context it is obvious why a bias is present for small samples.  The standard error of the coefficient is high therefore allowing greater deviations from the parameter.  By taking a maximum we capitalize on the greatest of these deviations thus biasing the statistic.  If one desires to be conservative then the minimum of the Lambda4 vector would be most appropriate.

}
\details{
\tabular{ll}{
Package: \tab Lambda4\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-03-12\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tyler Hunt

Maintainer: tyler.hunt@utah.edu
}
\references{
Osburn, H. (2000). Coefficient alpha and related internal consistency reliability coefficients. Psychological Methods, 5:343-355. \\
Guttman, L. (1945). A basis for analyzing test retest reliability. Psychometrika, 10:255-282.
}
\keyword{package maximized lambda4 reliability greatest lower bound
 maximal reliability}
\seealso{psych package
}
\examples{lambda4(USJudgeRatings)
}