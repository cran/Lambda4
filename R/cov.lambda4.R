cov.lambda4 <-
function (x, show.lambda4s=FALSE, show.splits=TRUE)
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
 

 
 if(show.splits==TRUE){
 sl4<-sort(l4.vect)	
 Min.Split<-t1t.matrix[which(l4.vect==sl4[1]),]	
 Median.Split<-t1t.matrix[which(l4.vect==sl4[round(lencombs/2)]),]	
 Max.Split<-t1t.matrix[which(l4.vect==sl4[lencombs]),]
 Splits<-data.frame(Min.Split, Median.Split, Max.Split)	
 }
 Max<-max(l4.vect)
 Mean<-mean(l4.vect)
 Median<-median(l4.vect)
 Minimum<-min(l4.vect)
 
 
 lambda4s<-lencombs
 Items<-nvar
 
 lambda4<-data.frame(Mean, Max, Median, Minimum)
 Analysis.Details<-data.frame(Items, lambda4s)
 
if (show.lambda4s==FALSE){
	if(show.splits==TRUE){result<-list(lambda4=lambda4, Analysis.Details=Analysis.Details, Splits=Splits)}
 	else
 	{result<-list(lambda4=lambda4, Analysis.Details=Analysis.Details)}
 }
 if(show.lambda4s==TRUE){
 	if(show.splits==TRUE){result<-list(lambda4=lambda4,Analysis.Details=Analysis.Details, l4.vect=l4.vect, Splits=Splits)}
 	else
 {result<-list(lambda4=lambda4,Analysis.Details=Analysis.Details, l4.vect=l4.vect)}
 }
 return(result)
 }
