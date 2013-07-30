\name{guttman}
\alias{guttman}
\title{Guttman's 6 Lambda Coefficients}
\usage{
  guttman(x, missing = "complete", standardize = FALSE)
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{missing}{How to handle missing values.}

  \item{standardize}{When TRUE Results are standardized by
  using the correlation matrix instead of the covariance
  matrix for computation.}
}
\value{
  \item{Lambda1}{Guttman's Lambda1 estimate of
  reliability.} \item{Lambda2}{Guttman's Lambda2 estimate
  of reliability.} \item{Lambda3}{Guttman's Lambda3
  estimate of reliability. Also known as Cronbach's alpha
  or coefficient alpha.} \item{Lambda4}{Guttman's maximimal
  Lambda4 estimate of reliability.}
  \item{Lambda5}{Guttman's Lambda5 estimate of
  reliability.} \item{Lambda6}{Guttman's Lambda6 estimate
  of reliability.}
}
\description{
  Calculates all 6 of Guttman's lambda coefficients.
}
\note{
  The estimate for Lambda4 is maximized.
}
\examples{
guttman(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Guttman L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

