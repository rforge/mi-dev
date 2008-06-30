\name{glm.mi}
\alias{glm.mi}
\title{ Generalized Linear Modeling Function for Multiply Imputed Dataset }
\description{
  Generalized Linear Modeling Function that pulls togather the estimates from multiply imputed dataset.
}
\usage{
glm.mi(formula, mi.object, family = gaussian, ...)
}
\arguments{
  \item{formula}{ See glm for detail. }
  \item{mi.object}{ mi object. }
  \item{family}{See glm for detail.}
  \item{\dots}{ Any option to pass on to glm function. }
}
\details{
  See glm.
}
\value{
  \item{call}{the matched call.}
  \item{glm.mi.pooled}{Pulled estimates from the multiple dataset.}
  \item{glm.mi.fit}{Estimates from each dataset. See glm for details.}

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
