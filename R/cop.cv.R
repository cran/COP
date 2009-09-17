cop.cv<-function(x,y,my.sel,K.fold,KK){
	set.seed(1234)
	n=nrow(x)
	my.label<-rmultinom(n,1,prob=rep(1,K.fold)/K.fold)
#	while(min(apply(my.label,1,sum))>2){
#	 my.label<-rmultinom(n,1,prob=rep(1,K.fold)/K.fold)
#	}
	my.sel.ind=my.sel
	x=as.matrix(x[,my.sel.ind])
	p=ncol(x)
	my.cv<-NULL
		for(k in 1:K.fold){
			xtest=x[my.label[k,]==1,]
			xtrain=x[my.label[k,]!=1,]
			ytest=y[my.label[k,]==1]
			ytrain=y[my.label[k,]!=1]
			train=data.frame(ytrain=ytrain,xtrain=xtrain)
			beta.hat=dr(ytrain~xtrain,data=train)$evectors[,1:KK]
			my.dim.cv=NULL		
			if(KK==1){
				temp=xtrain%*%beta.hat
				my.fit=loess(temp~ytrain,data=train)
				my.pred=predict(my.fit,data.frame(ytrain=ytest))
				if(sum(is.na(my.pred))!=0){
					my.del=which(is.na(my.pred))
					test=list(my.pred=my.pred[-my.del],xtest=xtest[-my.del,])
				}else{
					test=list(my.pred=my.pred,xtest=xtest)
					}
				my.dim.cv=cor(test[[1]], as.vector(test[[2]]%*%beta.hat))
			}else{
				for(m in 1:KK){	
					temp=xtrain%*%beta.hat[,m]
					my.fit=loess(temp~ytrain,data=train)
					my.pred=predict(my.fit,data.frame(ytrain=ytest))
					if(sum(is.na(my.pred))!=0){
						my.del=which(is.na(my.pred))
						test=list(my.pred=my.pred[-my.del],xtest=xtest[-my.del,])
					}else{
						test=list(my.pred=my.pred,xtest=xtest)
						}				
					my.dim.cv[m]=cor(test[[1]], as.vector(test[[2]]%*%beta.hat[,m]))^2			
					}			
				}
			print(my.dim.cv)
			my.cv[k]=sum(my.dim.cv)
			}
	K.CV=sum(my.cv)
	return(K.CV)	
	}		

