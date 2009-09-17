\name{backward.corp}
\alias{backward.corp}
\title{
Backward selection step in COP
}
\description{
This is the function for backward selection in COP
}
\usage{
backward.corp(x, y, id1, ind, H, signif.out, dd, lambdaf)
}
\arguments{
  \item{x}{is a n by p matrix for the covariates
}
  \item{y}{
is a response variable
}
  \item{id1}{
is the subset of variable selected at previous step
}
  \item{ind}{
is the subset of variable that has not been selected at previous step
}
  \item{H}{
is the number of slices
}
  \item{signif.out}{
is the threshold to kick a variable out from the selected subset of variables
}
  \item{dd}{is the direction
}
  \item{lambdaf}{
the critical value based on the full model
}
}

\value{
}
\references{
}
\author{
Wenxuan Zhong
}
\note{}
\seealso{
}
\examples{}
