chisq.in <-
function(lambdaf,lambdar,n){
        my.test=n*(lambdaf-lambdar)/(1-lambdaf)
	  pvalue<-1-pchisq(my.test,1)
	  return(pvalue)
}

