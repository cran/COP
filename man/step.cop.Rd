\name{step.cop}
\alias{step.cop}
\title{stepwise variable selection procedure using correlation pursuit}
\description{
This function perform the stepwise variable selection for fixed number of principle directions and a pre-defined threshold for adding and deleting variable from the selected subset of variables
} 
\usage{
 step.cop(x,y,H,alpha.in,alpha.out,my.range,k)
}
\arguments{
  \item{x}{is a n by p matrix of predictors
}
  \item{y}{
is a response variable
}
  \item{H}{
is the number of slices
}
  \item{alpha.in}{
is the threshold to add a significant variable in the selected subset of variables
}
  \item{alpha.out}{
is the threshold to delete a redundant variable from the selected subset of variables, alpha.out has to be smaller than alpha.in}

  \item{my.range}{is maximum number of variables that will be selected
}
  \item{k}{is the number of principle directions
}
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
my.cop.sel<-step.cop(x,y,5,0.95,0.90,8,1)
}

