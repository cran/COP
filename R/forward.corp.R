forward.corp <-
function(x,y,ind,H,signif.in,dd,lambdar){
        p=NCOL(x)
        n=nrow(x)
        set.all<-1:p
        set.dd<-setdiff(set.all,ind)
        pp=length(set.dd)        
        aa=NULL
        for(l in 1:pp){
                ind1<-c(ind,set.dd[l])
                xnew=x[,ind1]
                temp<-dr(y~scale(xnew),nslices=H)
                aa[l]<-temp$evalues[dd]
        }
        lambdaf<-max(aa)
	  sel<-which.max(aa)	
        if(chisq.in(lambdaf,lambdar,n)<signif.in){
                my.stop="conti"
		    id<-c(ind,set.dd[sel])	
        }else{
                id<-ind
                my.stop="stop"
        }
        return(list(id=id,my.stop=my.stop,lambda=lambdaf))
}

