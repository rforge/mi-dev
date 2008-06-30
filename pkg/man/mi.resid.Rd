\name{mi.resid}
\alias{mi.resid}
\alias{mi.resid,mi.method-method}
\alias{mi.resid,mi.dichotomous-method}
\alias{mi.resid,mi.logcontinuous-method}
\alias{mi.resid,mi.mixed-method}
\alias{mi.resid,mi.sqrtcontinuous-method}
\title{ Expected value from mi.method objec class. }
\description{
  Internal function to extract residual values from mi.method objects.
}
\usage{
mi.resid(object, \dots)
  \S4method{mi.resid}{mi.method}( object )
  \S4method{mi.resid}{mi.mixed}( object )
  \S4method{mi.resid}{mi.dichotomous}( object )
  \S4method{mi.resid}{mi.logcontinuous}( object )
  \S4method{mi.resid}{mi.sqrtcontinuous}( object )
}
\arguments{
  \item{object}{ mi.method object class. }
  \item{\dots}{ currently not used. }
}
\details{
  Extracs the residual values from mi.method object family.
}
\value{
  \item{resid}{residual value}
}
\references{
  Andrew Gelman and M. Grazia Pittau, A flexible program for missing-data imputation and model checking, Technical report, Columbia University, New York. 
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
