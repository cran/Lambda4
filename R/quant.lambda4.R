quant.lambda4<-function(x, starts=1000, quantile=.5, show.splits=FALSE){

#Outerloop

l4.vect<-rep(NA, starts)

#Determines if x is a covariance or data matrix and establishes a covariance amtrix for estimation.
p <- dim(x)[2]
 if (dim(x)[1] == p) sigma <- as.matrix(x)  else sigma <- var(x, use="pairwise")

items<-ncol(sigma)

#Creates an empty matrix for the minimized tvectors
splitmtrx<-matrix(NA, nrow=items, ncol=starts)

# creates the 1 row and column vectors for the lambda4 equation.
onerow<-rep(1,items)
onerow<-t(onerow)
onevector<-t(onerow)
f<-rep(NA,starts)
for(y in 1:starts){

#Innerloop (minimization function)


#start value for the minimization function



#Random number generator for the t-vectors
trow<-runif(items,0,100)
for(a in 1:items){
	if(trow[a] <  50)(trow[a]= -1)
	if(trow[a] >= 50)(trow[a]=  1)
}
trow<-t(trow)
tvector<-t(trow)

#Creating t1s for the innerloop
tk1<-(tvector)
tk1t<-t(tk1)
tk2<-(trow)
tk2t<-t(tk2)

#Establishes whether items should change sign or stay the same.  Thus minimizing the numerator.
sigma0<-sigma
random.order<-sample(1:items)
for (i in 1:items) {
	sigma0[i,i]<-c(0)}
for (o in 1:items){
oi<-sigma0[,random.order[o]]
fi<-oi%*%tk1
if (fi <  0) {tk1[random.order[o],1]<-  1}
if (fi >= 0) {tk1[random.order[o],1]<- -1}
}

t1<-(1/2)*(tk1+1)
fk1<-tk1t%*%sigma0%*%tk1
t1t<-t(t1)
t2<-(1-t1)
t2t<-t(t2)



#if the new f value is greater than the previous then the innerloop stops and records the previous t-vector.

#End of Innerloop
f[y]=fk1
splitmtrx[,y]<-t1


l4.vect[y]<-(4*(t1t%*%sigma%*%t2))/(onerow%*%sigma%*%onevector)
}
quants<-quantile(l4.vect, quantile)
lambda4.quantile=quants
lambda4.optimal<-max(l4.vect)
lambda4.alpha<-mean(l4.vect)
if (show.splits==FALSE){
	result<-list(lambda4.quantile=lambda4.quantile, lambda4.optimal=lambda4.optimal)
}
if(show.splits==TRUE){
	result<-list(lambda4.quantile=lambda4.quantile, lambda4.optimal=lambda4.optimal,l4.vect=l4.vect)
}

return(result)
}