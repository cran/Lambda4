cov.lambda4 <-function (x, show.splits=FALSE)
 {

 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #Determines if x is a covariance or a data matrix and establishes a covariance matrix for estimation.
 p <- dim(x)[2]
 if (dim(x)[1] == p) sigma <- as.matrix(x)  else sigma <- var(x, use="pairwise")

 sigma.split<-as.data.frame(sigma)
 
 #replacing the diagonal and the upper diagonal with 0s
 
 sigma.split[upper.tri(sigma.split, diag=TRUE)]<-0
 
 sigma.split<-as.data.frame(sigma.split)
 
 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 		
 #finding row and column of the max value in var/cov matrix and then replacing entire row and #columns with 0s
 
 xy<-matrix(ncol=2,nrow=nvar/2)
 		
 		for(o in 1:(nvar/2)){
 			x.m<-which(sigma.split==max(sigma.split),arr.ind=TRUE)
 			xy[o,1]<-c(x.m[1])
 			xy[o,2]<-c(x.m[2])
 			sigma.split[(x.m[1]),]<-c(0)
 			sigma.split[(x.m[1])]<-c(0)
 			sigma.split[(x.m[2])]<-c(0)
 			sigma.split[(x.m[2]),]<-c(0)
 					}
 #list of first half of variables
 Ahalf<-xy[,1]
 #list of second half of variables
 Bhalf<-xy[,2]
 #determining if a value was lost from the matrix
 occ<-c(1:nvar)
 lst<-c(Ahalf,Bhalf)
 flse<-occ%in%lst
 lftout<-which(flse==FALSE)
 lth1<-length(occ)
 lth2<-length(lst)
 #adding the lost value if there are an odd number of items
 if(lth1>lth2)
 {Bhalf<-c(Bhalf,lftout)}
 
 #first half of data matrix
 A<-(x[,Ahalf])
 #second half of data matrix
 B<-(x[,Bhalf])
 
 Ani<-length(Ahalf)	
 test <-xy
 
  bincombinations<-function (p) 
{
    retval <- matrix(0, nrow = 2^p, ncol = p)
    for (n in 1:p) {
        retval[, n] <- rep(c(rep(0, (2^p/2^n)), rep(1, (2^p/2^n))), 
            length = 2^p)
    }
    retval
}
 combsall<-bincombinations(Ani)
 
 lencombs<-(nrow(combsall)/2)
 
 combs<-combsall[1:lencombs,]

 t1t.temp<-rep(1,nvar)
 for(z in 1:nvar){
 	temp<-Bhalf[z]
 	t1t.temp[temp]<-t1t.temp[temp]*0
 }
 
 for(a in 1:lencombs){
 	for(b in 1:Ani){
		if(combs[a,b]==0)	test[b,]<-c(xy[b,1],xy[b,2])
 		if(combs[a,b]==1)	test[b,]<-c(xy[b,2],xy[b,1])
 		Bhalft<-test[,2]
 		}
 		temp2<-rep(1,nvar)
 		for(z in 1:nvar){
 			temp<-Bhalft[z]
 			temp2[temp]<-temp2[temp]*0
 }
 t1t.temp<-rbind(t1t.temp,temp2)
 }
 t1t.temp<-t1t.temp[2:(lencombs+1),]
 
 l4.vect<-rep(0, lencombs)
 onerow<-rep(1,nvar)
 onerow<-t(onerow)
 onevector<-t(onerow)
 for (r in 1:lencombs){
 	t1t<-t1t.temp[r,]
 	t1t<-t(t1t)
 	t1<-t(t1t)
 	t2<-(1-t1)
 	l4.vect[r]<-(4*((t1t%*%sigma)%*%t2))/(onerow%*%sigma)%*%onevector
 }
 	
 max.lambda4<-max(l4.vect)
 mean.lambda4<-mean(l4.vect)
 median.lambda4<-median(l4.vect)
 min.lambda4<-min(l4.vect)
 
if (show.splits==FALSE){
 	result<-list(max.lambda4=max.lambda4, mean.lambda4=mean.lambda4, median.lambda4=median.lambda4, min.lambda4=min.lambda4)
 }
 if(show.splits==TRUE){
 result<-list(l4.vect=l4.vect, max.lambda4=max.lambda4, mean.lambda4=mean.lambda4, median.lambda4=median.lambda4, min.lambda4=min.lambda4)
 }
 return(result)
 }
