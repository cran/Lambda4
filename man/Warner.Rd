\name{Warner}
\alias{Warner}
\docType{data}
\title{
Warner 1960 Social Class Data
}
\description{Data comes from Warner, Meeker, and Eels (1960) and is a multidimensional scale of composite scores of social class.}
\usage{data(Warner)}
\format{
  The format is:
 num [1:6, 1:6] 1 0.87 0.76 0.71 0.7 0.77 0.87 1 0.82 0.81 ...
}
\details{ The components were averaged and then the averages were used for the covariance matrix.  The factors are: occupation, amount of income, source of income, house type, dwelling area, and education.  These data have been used by Bentler (1972) to show the stark difference between alpha and the glb.}
\source{
Bentler, PM (1972). "A Lower-Bound Method for the Dimension-Free Measurement of Internal Consistency" 1(4), 343-357.
}
\references{
Warner WL, Meeker M, and Eels K. (1960), "Social Class in America" Harper and Row:New York. 
}
\examples{
data(Warner)
}
\keyword{datasets}
