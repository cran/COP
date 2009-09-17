starting <-
function(x,y,H){
      n=nrow(x);p=ncol(x);
      aa<-NULL
      for(l in 1:p){
                slice.1<-sapply(split(scale(x[,l]),as.factor(dr.slices(y,nslices=H)[[1]])),mean,simplify=TRUE)
                aa[l]<-var(slice.1)
      }
	lambda0<-max(aa)
       id<-which.max(aa)
	 set.all<-1:p
       set.dd<-setdiff(set.all,id)
	 pp=length(set.dd) 
	 aa<-NULL
	 for(l in 1:pp){
                ind1<-c(id,set.dd[l])
                xnew=x[,ind1]
                temp<-dr(y~scale(xnew),nslices=H)
                aa[l]<-temp$evalues[1]
        }
	  id<-c(id,set.dd[which.max(aa)])	
        lambdar<-max(aa)
	return(list(id=id,lambda=lambdar,lambda0=lambda0))
}

