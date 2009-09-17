backward.corp <-
function(x,y,id1,ind,H,signif.out,dd,lambdaf){
        p=NCOL(x)
        n=nrow(x)
        pp=length(ind)
        aa=NULL
        for(l in 1:pp){
                ind1<-ind[-l]
                xnew=x[,c(id1,ind1)]
                temp<-dr(y~scale(xnew),nslices=H)
                aa[l]<-temp$evalues[dd]
        }
        lambdar<-max(aa)
	  sel<-which.max(aa)
        if(chisq.out(lambdaf,lambdar,n)<signif.out){
                id<-c(id1,ind[-sel])
		    lambdar<-lambdar	
                my.stop="conti"
        }else{
                id<-c(id1,ind)
		    lambdar<-lambdaf	
                my.stop="stop"
        }
        return(list(id=id,my.stop=my.stop,lambda=lambdar))
}

