\name{par1f}
\alias{par1f}
\docType{data}
\title{One Factor Parallel Covariance Matrix}
\description{
This Covariance matrix was used as the population model for one set of simulations.  It was used to represent a parallel data structure in which all factor loadings and error variances are set at .6.
}
\usage{data(par1f)}
\format{
  A covariance matrix of 8 theoretical items.
}
\examples{
###---Loadings
fx<-t(matrix(c(
.6,
.6,
.6,
.6,
.6,
.6,
.6,
.6), nrow=1))

###--Error Variances
err<-diag(c(.6^2,.6^2,.6^2,.6^2,
  		.6^2,.6^2,.6^2,.6^2))
		
###---matrix of factor covariances
phi<-matrix(1, nrow=1)

###---Reliability Calculation---###
t1<-matrix(c(rep(1,8)), nrow=1)
t1t<-matrix(c(rep(1,8)), ncol=1)

(fx\%*\%phi\%*\%t(fx)+err)
}
\keyword{datasets}
