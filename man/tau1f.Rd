\name{tau1f}
\alias{tau1f}
\docType{data}
\title{One-Factor Tau-Equivalent Covariance Matrix}
\description{
This covariance matrix was used as the population model for one set of simulations.  It was used to represent a tau equivalent data structure in which the factor loadings are set at .6.  The error variances were set at .6^2, .7^2, .8^2, .9^2, .6^2, .7^2, .8^2, and .9^2.
}
\usage{data(tau1f)}
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
err<-diag(c(.6^2,.7^2,.8^2,.9^2,
  		.6^2,.7^2,.8^2,.9^2))
		
###---matrix of factor covariances
phi<-matrix(1, nrow=1)

###---Reliability Calculation---###
t1<-matrix(c(rep(1,8)), nrow=1)
t1t<-matrix(c(rep(1,8)), ncol=1)

(fx\%*\%phi\%*\%t(fx)+err)
}
\keyword{datasets}
