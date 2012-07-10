cov.lambda4 <-
function (x, show.splits=FALSE)
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
 sigma.split2<-sigma-diag(sigma)
 		
 #finding row and column of the max value in var/cov matrix and then replacing entire row and #columns with -999999s
 
 xy<-matrix(ncol=2,nrow=nvar/2)
 		
 		for(o in 1:(nvar/2)){
 			x.m<-which(sigma.split==max(sigma.split),arr.ind=TRUE)
 			xy[o,1]<-x.m[1]
 			xy[o,2]<-x.m[2]
 			sigma.split[(x.m[1]),]<--999999
 			sigma.split[(x.m[1])]<--999999
 			sigma.split[(x.m[2])]<--999999
 			sigma.split[(x.m[2]),]<--999999
 					}
 #list of first half of variables
 Ahalf<-xy[,1]
 #list of second half of variables
 Bhalf<-xy[,2]
 #determining if a value was lost from the matrix
 items.seq<-seq(1:nvar)
 lst<-c(Ahalf,Bhalf)
 lftout<-which(items.seq%in%lst==FALSE)
 #adding the lost value if there are an odd number of items
 if(length(c(Ahalf,Bhalf))!=length(items.seq))
 	{Bhalf<-c(Bhalf,lftout)}
  
 Ani<-length(Ahalf)
 Bni<-length(Bhalf)
 Acombs<-bin.combs(Ani)	
 lencombs<-nrow(Acombs)
 
 t1t.temp<-(as.numeric(items.seq%in%Ahalf)-.5)*2
 t1t.splits<-t(matrix(data=rep(t1t.temp,lencombs),nrow=nvar, ncol=lencombs))
 
 full<-cbind(Acombs,Acombs)
 if(Ani!=Bni) 
 	{full<-cbind(full,rep(1,lencombs))}
 full[,c(Ahalf,Bhalf)]<-full[,seq(1:nvar)]
 
 if(Ani!=Bni)
 	{covt<-which(sigma.split2[lftout,]==max(sigma.split2[lftout,]))}
  if(Ani!=Bni)
 	{full[,lftout]<--t1t.temp[covt]}

 t1t.matrix<-(full*t1t.splits)/2+.5
 t2.matrix<-(t(t1t.matrix)-1)*-1

 onerow<-rep(1,ncol(t1t.matrix))
 onerow<-t(onerow)
 onevector<-t(onerow)
 
 l4.vect<-rep(NA, lencombs)
 for(i in 1:lencombs){
 l4.vect[i]<-(4*(t1t.matrix[i,]%*%sigma%*%t2.matrix[,i]))/(onerow%*%sigma)%*%onevector
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
