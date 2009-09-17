\name{cop.cv}
\alias{cop.cv}
\title{select the optimal variable in and variable out threshold in correlation pursuit using K fold cross validation}
\description{
This function calculate the K-fold CV for selecting the optimal variable-in and variable-out threshold in correlation pursuit method} 
\usage{
 cop.cv(x,y,my.sel,K.fold,KK)
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
  \item{K.fold}{
the number of fold in the CV calculation}
  \item{KK}{
specify the number of principle directions of the selected subset of variables}
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
my.cop.cv=NULL
for(i in 1:3){
my.cop.cv[i]=cop.cv(x,y,my.cop.sel[[i]],10,1)
}
my.cop.cv
}

