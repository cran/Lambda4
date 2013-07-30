\name{par3f}
\alias{par3f}
\docType{data}
\title{Three-Factor Parallel Covariance Matrix}
\description{
This Covariance matrix was used as the population model for one set of simulations.  It was used to represent a parallel data structure in which all factor loadings and error variances are set at .6 and the latent variables are correlated at .3.
}
\usage{data(par3f)}
\format{
  A covariance matrix of 12 theoretical items.
}
\examples{
###---Loadings
fx<-t(matrix(c(
.6,0,0,
.6,0,0,
.6,0,0,
.6,0,0,
0,.6,0,
0,.6,0,
0,.6,0,
0,.6,0,
0,0,.6,
0,0,.6,
0,0,.6,
0,0,.6), nrow=3))

###--Error Variances
err<-diag(c(  .6^2,.6^2,.6^2,.6^2,
				.6^2,.6^2,.6^2,.6^2,
				.6^2,.6^2,.6^2,.6^2))
		
###---3x3 matrix of factor covariances
phi<-matrix(c(rep(.3, 9)), nrow=3)
diag(phi)<-1

###---Reliability Calculation---###
t1<-matrix(c(rep(1,12)), nrow=1)
t1t<-matrix(c(rep(1,12)), ncol=1)

(fx\%*\%phi\%*\%t(fx)+err)
}
\keyword{datasets}
