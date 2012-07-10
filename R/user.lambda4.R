user.lambda4 <-
function(x, split="even odd"){

 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #Determines if x is a covariance or a data matrix and establishes a covariance matrix for estimation.
 p <- dim(x)[2]
 if (dim(x)[1] == p) sigma <- as.matrix(x)  else sigma <- var(x, use="pairwise")
 if(split[1]=="even odd") t1t.split<-rep(c(1,0),ceiling(nvar/2))[1:nvar] else t1t.split<-split
 
 if(length(t1t.split)!=nvar)
 	warning("The length of split is not the same as the number of items")
 t1t.split<-t(t1t.split)
 t2.split<-(t(t1t.split)-1)*-1
 
 onerow<-rep(1, nvar)
 onerow<-t(onerow)
 onevector<-t(onerow)
 
 Estimate<-(4*(t1t.split%*%sigma%*%t2.split))/(onerow%*%sigma)%*%onevector
 Estimate<-data.frame(Estimate, row.names="")
 result<-list(Lambda4=Estimate)
 return(result)
 }
