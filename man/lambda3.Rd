\name{lambda3}
\alias{lambda3}
\title{Compute Guttman's Lambda 3 Coefficient (Coefficent Alpha)}
\usage{
  lambda3(x, item.stats.max = 12, missing = "complete")
}
\arguments{
  \item{x}{Can be either a data matrix or a covariance
  matrix}

  \item{item.stats.max}{items statistics shown if the
  number of items are less than this value.}

  \item{missing}{how to handle missing values.}
}
\value{
  \item{lambda3}{The unstandardized and standardized
  lambda3 estimate.} \item{item.stats}{If the input data
  was a covariance matrix then this is a table of
  reliability estimates if an item was dropped.  If the
  input data is a data frame then the mean, standard
  deviation, and number of observations are also included.}
  \item{items}{The number of items.}
  \item{item.stats.max}{The maximum number of item to
  display the item.stats table (user specified).}
}
\description{
  Often recognized as Cronbach's alpha, Guttman's Lambda 3
  can be used to estimate reliability when the data can be
  split in parallel forms.
}
\examples{
lambda3(Rosenberg)
}
\author{
  Tyler Hunt \email{tyler@psychoanalytix.com}
}
\references{
  Cronbach L (1951). "Coefficient Alpha and the Internal
  Structure of Tests." Psychometrika, 16, 297-334. Guttman
  L (1945). "A Basis for Analyzing Test-Retest
  Reliability." Psychometrika, 10, 255-282.
}

