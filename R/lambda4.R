lambda4 <-
function (x)
 {
 	
 if (!is.matrix(x) && !is.data.frame(x)) 
         stop("Data must either be a data frame or a matrix")
 
 #creating variance covariance matrix
 Vmtx<-var(x, use="pairwise")
 
 
 Vmtx<-as.data.frame(Vmtx)
 
 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #replacing the diagonal and the upper diagonal with 0s
 	for (i in 1:nvar) {
 		Vmtx[i,i]<-c(0)
 
 
  k=i-1
          for (j in 1:k) {
               Vmtx[j,i] <- c(0)
 					}}
 					Vmtx<-var(x, use="pairwise")
 
 
 Vmtx<-as.data.frame(Vmtx)
 
 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #replacing the diagonal and the upper diagonal with 0s
 	for (i in 1:nvar) {
 		Vmtx[i,i]<-c(0)
 
 
  k=i-1
          for (j in 1:k) {
               Vmtx[j,i] <- c(0)
 					}}
 					
 #finding row and column of the max value in var/cov matrix and then replacing entire row and #columns with 0s
 
 xy<-matrix(ncol=2,nrow=nvar/2)
 		
 		for(o in 1:(nvar/2)){
 			x.m<-which(Vmtx==max(Vmtx),arr.ind=TRUE)
 			xy[o,1]<-c(x.m[1])
 			xy[o,2]<-c(x.m[2])
 			Vmtx[(x.m[1]),]<-c(0)
 			Vmtx[(x.m[1])]<-c(0)
 			Vmtx[(x.m[2])]<-c(0)
 			Vmtx[(x.m[2]),]<-c(0)
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
 #Returns a matrix containing the 2^p vectors of length p.
 #Taken from package e1071
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
 
 
 #creating lambda 4 and setting it at zero
 lambda4.list<-rep(0, lencombs)
 #Maximizing function that calculates split half for every combination
 for(a in 1:lencombs){
 	for(b in 1:Ani){
		if(combs[a,b]==0)	test[b,]<-c(xy[b,1],xy[b,2])
 		if(combs[a,b]==1)	test[b,]<-c(xy[b,2],xy[b,1])
 		}
 		#list of first half of variables
 		Ahalft<-test[,1]
 		#list of second half of variables
 		Bhalft<-test[,2]
 			if(lth1>lth2)
 				{Ahalft<-c(Ahalft,lftout)}
 							
 		#first half of data matrix
 		Asplit<-(x[,Ahalft])
 		#second half of data matrix
 		Bsplit<-(x[,Bhalft])
 		Atest<-rep(0,n)
 			for(u in 1:n){
 				Atest[u]<-sum(Asplit[u,], na.rm=TRUE)
 						}
 		#sum of variable scores for the second half
 		Btest<-rep(0,n)
 			for(u in 1:n){
 				Btest[u]<-sum(Bsplit[u,], na.rm=TRUE)
 						}
 							
 						r.test<-cor(Atest,Btest)
 						lambda4.list[a]<-(2*r.test)/(1+r.test)
}
 max.lambda4<-max(lambda4.list)
 mean.lambda4<-mean(lambda4.list)
 median.lambda4<-median(lambda4.list)
 min.lambda4<-min(lambda4.list)
 
 result<-list(max.lambda4=max.lambda4, lambda4.list=lambda4.list, mean.lambda4=mean.lambda4, median.lambda4=median.lambda4, min.lambda4=min.lambda4)
 return(result)
 					}
