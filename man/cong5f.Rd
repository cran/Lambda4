\name{cong5f}
\alias{cong5f}
\docType{data}
\title{Five-Factor Congeneric Covariance Matrix}
\description{
This Covariance matrix was used as the population model for one set of simulations.  It was used to represent a congeneric data structure in which the factor loadings are set at .5, .6, .7, .8, .5, .6, .7, and .8.  The error variances were set at .6^2, .7^2, .8^2, .9^2, .6^2, .7^2, .8^2, and .9^2.  The correlations between the latent variables was fixed to .3.
}
\usage{data(cong5f)}
\format{
  A covariance matrix of 20 theoretical items.
}
\examples{
fx<-t(matrix(c(
.5,0,0,0,0,
.6,0,0,0,0,
.7,0,0,0,0,
.8,0,0,0,0,
0,.5,0,0,0,
0,.6,0,0,0,
0,.7,0,0,0,
0,.8,0,0,0,
0,0,.5,0,0,
0,0,.6,0,0,
0,0,.7,0,0,
0,0,.8,0,0,
0,0,0,.5,0,
0,0,0,.6,0,
0,0,0,.7,0,
0,0,0,.8,0,
0,0,0,0,.5,
0,0,0,0,.6,
0,0,0,0,.7,
0,0,0,0,.8), nrow=5))

###--Error Variances
err<-diag(c(.6^2,.7^2,.8^2,.9^2,
  	.6^2,.7^2,.8^2,.9^2,
		.6^2,.7^2,.8^2,.9^2,
		.6^2,.7^2,.8^2,.9^2,
		.6^2,.7^2,.8^2,.9^2))
		
###---5x5 matrix of factor covariances
phi<-matrix(c(rep(.3, 25)), nrow=5)
diag(phi)<-1

###---Reliability Calculation---###
t1<-matrix(c(rep(1,20)), nrow=1)
t1t<-matrix(c(rep(1,20)), ncol=1)

(fx\%*\%phi\%*\%t(fx)+err)
}
\keyword{datasets}
