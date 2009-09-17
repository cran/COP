GIC<-function(x,y,my.sel,KK){
	x1=x[,my.sel]
	p=ncol(x1)
	n=nrow(x1)
	phi=dr(y~x[,my.sel])$M
	omega=phi+diag(1,p)
	tao=length(eigen(omega)$values>1)
	ss=min(tao,KK)
	theta=eigen(omega)$values[(1+ss):p]
	logL=n/2*sum(log(theta)+1-theta)
	Gk=-(logL-2*KK*(log(n)*p-KK+1)/2)
	return(Gk)
	}		


