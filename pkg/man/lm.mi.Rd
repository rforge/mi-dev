\name{lm.mi}
\alias{lm.mi}
\title{ Linear Regression Function for Multiply Imputed Dataset }
\description{
  Linear Regression Function that pulls togather the estimates from multiply imputed dataset.
}
\usage{
lm.mi(formula, mi.object, ...)
}
\arguments{
  \item{formula}{ View lm for detail. }
  \item{mi.object}{ mi object. }
  \item{\dots}{ Any option to pass on to lm function. }
}
\details{
  Extracs the starting values from mi.method object family.
}
\value{
  \item{call}{the matched call.}
  \item{lm.mi.pooled}{Pulled estimates from the multiple dataset.}
  \item{lm.mi.fit}{Estimates from each dataset. See lm for details.}

}
\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}
\seealso{
  \code{\link{mi.models}} 
}
\keyword{ models }
