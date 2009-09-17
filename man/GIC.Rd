\name{GIC}
\alias{GIC}
\title{select the optimal number of principle directions using GIC function in correlation pursuit algorithm}
\description{
This function can estimate the best number of principle directions by minimizing the GIC function} 
\usage{
 GIC(x,y,my.sel,KK)
}
\arguments{
  \item{x}{is a n by p matrix of predictors.
}
  \item{y}{
is a response variable.
}
  \item{my.sel}{
is the selected subset of variables obtained using step.cop function.
}
  \item{KK}{
specify the candidate number of principle directions}
}
\author{
Wenxuan Zhong
}

\seealso{
\code{\link{cop.cv}}
}
\examples{
## generate data with n=200 and p=8
beta<-c(3,1.5,1,1,2,0,0,0)
x<-mvrnorm(200,rep(0,8),diag(1,8))
y<-x\%*\%beta+rnorm(200)
alpha.in=c(0.9,0.95,0.99)
i=1
my.cop.sel=list()
while(i <=3){
my.cop.sel[[i]]<-step.cop(x,y,5,alpha.in[i],alpha.in[i]-0.05,8,1)
i=i+1
}
my.d=NULL
for(i in 1:3){
my.d[i]=GIC(x,y,my.cop.sel[[i]],i)
}
my.d
}

