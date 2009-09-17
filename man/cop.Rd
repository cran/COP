\name{cop}
\alias{cop}
\title{
This function is for selecting variables for index models
}
\description{
This packages is for selecting variables for index models using correlation pursuit method. 
Correlation pursuit (COP) can be viewed as a
generalization of the conventional linear stepwise regression method 
to semi-parametric regression models. Unlike the
conventional stepwise, COP selects variables that maximize the
correlation between a transformed response and a linear
combination of the predictors. A sequential selection strategy is
used to select variables on multiple linear combinations.
}
\usage{
cop(x, y, signif.in, signif.out, H, my.range)
}
\arguments{
  \item{x}{
is a n by p matrix for the covariates
}
  \item{y}{
is a response variable
}
  \item{signif.in}{
is the p-value to add a new variable to the current selected subset of variables
}
  \item{signif.out}{
is the p-value to delete a variable from the current selected subset of variables
}
  \item{H}{
is the number of slices
}
  \item{my.range}{
is the maximum number of variables that you want to selected
}
}
\value{
return a list with 3 components
\item{id}{The label of variables that selected}
\item{lambda }{The eigenvalues that obtained from each step}
\item{my.sel}{The variables selected from each direction}
}
\references{Zhong, W. Zhang, T. Zhu, Y. Liu, J.S.(2009) Correlation pursuit: Stepwise Variable Selection for Index Models, see also
http://www.stat.uiuc.edu/~wenxuan/paper/juncopcombine2.pdf
}
\author{
Wenxuan Zhong
}
\seealso{}
\examples{
x=mvrnorm(40,rep(0,8),diag(1,8))
beta=c(3,1.5,0,0,2,0,0,0)
y=x\%*\%beta+0.1*rnorm(40,0,1)

my.cop=cop(x,y,signif.in=0.01,signif.out=0.05,H=4,my.range=8)

}

