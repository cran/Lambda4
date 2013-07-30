\name{omega.tot}
\alias{omega.tot}
\title{Compute McDonald's Omega Total}
\usage{
  omega.tot(x, factors = 1, missing = "complete")
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{missing}{how to handle missing values. \eqn{mi}.}

  \item{factors}{The number of latent factors.}
}
\value{
  \item{omega.tot}{Omega total reliability estimate.}
}
\description{
  McDonald proposed Omega Total as a method for estimating
  reliabilty for a test with multiple factors.
}
\examples{
omega.tot(Rosenberg, factors=1)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  McDonald, R. P. (1999). Test Theory: Aunified Treatment.
  Psychology Press.
}

