chisq.out <-
function(lambdaf,lambdar,n){
        my.test=n*(lambdaf-lambdar)/(1-lambdaf)
	  pvalue<-pchisq(my.test,1)
	  return(pvalue)
}

