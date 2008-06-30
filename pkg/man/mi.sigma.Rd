\name{mi.sigma}
\alias{mi.sigma}
\alias{mi.sigma,mi.method-method}
\alias{mi.sigma,mi.mixed-method}
\title{ Expected value from mi.method objec class. }
\description{
  Internal function to extract estimated variance from mi.method objects.
}
\usage{
mi.sigma(object,\dots)
  \S4method{mi.sigma}{mi.method}( object )
  \S4method{mi.sigma}{mi.mixed}( object )
}
\arguments{
  \item{object}{ mi.method object class. }
  \item{\dots}{ currently not used. }
}
\details{
  Extracs the estimated variance from mi.method object family.
}
\value{
  \item{sigma}{estimated variance.}
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
