user.lambda4 <-
function(x, split.method="even.odd", bootstrap=FALSE, B=1000, show.boots=FALSE, item.stats.max=12){

 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #Determines if x is a covariance or a data matrix and establishes a covariance matrix for estimation.
 p <- dim(x)[2]
 if (dim(x)[1] == p) sigma <- as.matrix(x)  else sigma <- var(x, use="pairwise")
 if(split.method[1]=="even.odd") t1t.split<-rep(c(1,0),ceiling(nvar/2))[1:nvar] 
 if(split.method[1]=="random") t1t.split<-round(runif(nvar))
 if(split.method[1]=="evenly.random") t1t.split<-sample(rep(c(1,0),ceiling(nvar/2))[1:nvar])
 if(split.method[1]==1 | split.method[1]==0) t1t.split<-split.method 
 if(length(t1t.split)!=nvar)
 	warning("The length of split is not the same as the number of items")
 Split<-t1t.split
 Obs<-colSums(!is.na(x))
 if(p<=item.stats.max){
 if (dim(x)[1] != p)
 	{
 	Mean<-round(colMeans(x, na.rm=TRUE),digits=2)
 	SD<-round(sapply(x,sd, na.rm=TRUE), digits=2)
 	Item.Statistics<-data.frame(Split,Mean,SD,Obs, row.names=(colnames(x)))
	 }
else
	{Item.Statistics<-data.frame(Split, Obs)}}
 t1t.split<-t(t1t.split)
 t2.split<-(t(t1t.split)-1)*-1
 
 onerow<-rep(1, nvar)
 onerow<-t(onerow)
 onevector<-t(onerow)
 if(bootstrap==TRUE){
 	temp<-rep(NA, B)
 	for(i in 1:B){
 		samp<-round(sample(1:n, replace=TRUE))
 		sigma<-cov(x[samp,], use="pairwise")
 		temp[i]<-(4*(t1t.split%*%sigma%*%t2.split))/(onerow%*%sigma)%*%onevector
 		
 	}
 	LowerCI<-quantile(temp,.45)
 	UpperCI<-quantile(temp,.55)
 	Estimate<-mean(temp)
 	Estimate<-data.frame(LowerCI,Estimate,UpperCI, row.names=NULL)
 	Boots<-temp
 }
 else {Estimate<-(4*(t1t.split%*%sigma%*%t2.split))/(onerow%*%sigma)%*%onevector 
 	Estimate<-data.frame(Estimate)}
 
 if(p<=item.stats.max){
 if(show.boots==TRUE) 
 {result<-list(lambda4=Estimate, Item.Statistics=Item.Statistics, Boots=Boots)}
 else
 {result<-list(lambda4=Estimate, Item.Statistics=Item.Statistics)}}
 else
 {result<-list(lambda4=Estimate)}
 return(result)
 }
