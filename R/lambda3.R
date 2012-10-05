lambda3 <-
function(x, item.stats.max=12){
	
 #number of variables
 p <-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #Determines if x is a covariance or a data matrix and establishes a covariance matrix for estimation.
 if (dim(x)[1] == p) sigma <- as.matrix(x)  else sigma <- var(x, use="pairwise")
 
 sigma.cor<-cov2cor(sigma)
 
 Obs<-colSums(!is.na(x))
 Mean<-round(colMeans(x, na.rm=TRUE),digits=2)
 SD<-round(sapply(x, sd, na.rm=TRUE), digits=2)
 
 onerow<-rep(1,p)
 onerow<-t(onerow)
 onevector<-t(onerow)
 
 Unstandardized<-(p/(p-1))*(1-(onerow%*%diag(sigma)/(onerow%*%sigma%*%onevector)))
 Standardized<-(p/(p-1))*(1-(onerow%*%diag(sigma.cor)/(onerow%*%sigma.cor%*%onevector)))
 Items<-p
 lambda3<-data.frame(Unstandardized, Standardized, Items)
 
 If.Dropped<-rep(NA,p)
 for(i in 1:p){
 onerow.d<-rep(1,(p-1))
 onerow.d<-t(onerow.d)
 onevector.d<-t(onerow.d)
 	sigma.d<-sigma[-i,-i]
 	If.Dropped[i]<-(p/(p-1))*(1-(onerow.d%*%diag(sigma.d)/(onerow.d%*%sigma.d%*%onevector.d)))
 }
 if(Items <= item.stats.max) {
 Item.Statistics<-data.frame(Mean,SD,Obs,If.Dropped, row.names=(colnames(x))) 
 result<-list(lambda3=lambda3, Item.Statistics=Item.Statistics) }
 
 else {result<-list(lambda3=lambda3)}
 
 return(result)
}
