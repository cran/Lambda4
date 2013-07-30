\name{tau5f}
\alias{tau5f}
\docType{data}
\title{Five-Factor Tau-Equivalent Covariance Matrix}
\description{
This covariance matrix was used as the population model for one set of simulations.  It was used to represent a tau equivalent data structure in which the factor loadings are set at .6.  The error variances were set at .6^2, .7^2, .8^2, .9^2, .6^2, .7^2, .8^2, and .9^2. The correlations between the latent variables was fixed to .3.
}
\usage{data(tau5f)}
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
