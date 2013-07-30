\name{cong3f}
\alias{cong3f}
\docType{data}
\title{Three-Factor Congeneric Covariance Matrix}
\description{
This covariance matrix was used as the population model for one set of simulations.  It was used to represent a congeneric data structure in which the factor loadings are set at .5, .6, .7, .8, .5, .6, .7, and .8.  The error variances were set at .6^2, .7^2, .8^2, .9^2, .6^2, .7^2, .8^2, and .9^2.  The correlations between the latent variables was fixed to .3.
}
\usage{data(cong3f)}
\format{
  A covariance matrix of 12 theoretical items.
}
\examples{
###---Loadings
fx<-t(matrix(c(
.5,0,0,
.6,0,0,
.7,0,0,
.8,0,0,
0,.5,0,
0,.6,0,
0,.7,0,
0,.8,0,
0,0,.5,
0,0,.6,
0,0,.7,
0,0,.8), nrow=3))

###--Error Variances
err<-diag(c(.6^2,.7^2,.8^2,.9^2,
  	.6^2,.7^2,.8^2,.9^2,
		.6^2,.7^2,.8^2,.9^2))
		
###---3x3 matrix of factor covariances
phi<-matrix(c(rep(.3, 9)), nrow=3)
diag(phi)<-1

###---Reliability Calculation---###
t1<-matrix(c(rep(1,12)), nrow=1)
t1t<-matrix(c(rep(1,12)), ncol=1)

(fx\%*\%phi\%*\%t(fx)+err)
}
\keyword{datasets}
