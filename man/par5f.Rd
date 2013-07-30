\name{par5f}
\alias{par5f}
\docType{data}
\title{Five-Factor Parallel Covariance Matrix}
\description{
This Covariance matrix was used as the population model for one set of simulations.  It was used to represent a parallel data structure in which all factor loadings and error variances are set at .6 and the latent variables are correlated at .3.
}
\usage{data(par5f)}
\format{
  A covariance matrix of 20 theoretical items.
}
\examples{
###---Loadings
fx<-t(matrix(c(
.6,0,0,0,0,
.6,0,0,0,0,
.6,0,0,0,0,
.6,0,0,0,0,
0,.6,0,0,0,
0,.6,0,0,0,
0,.6,0,0,0,
0,.6,0,0,0,
0,0,.6,0,0,
0,0,.6,0,0,
0,0,.6,0,0,
0,0,.6,0,0,
0,0,0,.6,0,
0,0,0,.6,0,
0,0,0,.6,0,
0,0,0,.6,0,
0,0,0,0,.6,
0,0,0,0,.6,
0,0,0,0,.6,
0,0,0,0,.6), nrow=5))

###--Error Variances
err<-diag(c(.6^2,.6^2,.6^2,.6^2,
  	.6^2,.6^2,.6^2,.6^2,
		.6^2,.6^2,.6^2,.6^2,
		.6^2,.6^2,.6^2,.6^2,
		.6^2,.6^2,.6^2,.6^2))
		
###---5x5 matrix of factor covariances
phi<-matrix(c(rep(.3, 25)), nrow=5)
diag(phi)<-1

###---Reliability Calculation---###
t1<-matrix(c(rep(1,20)), nrow=1)
t1t<-matrix(c(rep(1,20)), ncol=1)

(fx\%*\%phi\%*\%t(fx)+err)
}
\keyword{datasets}
