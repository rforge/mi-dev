\name{mi.coef}
\alias{mi.coef}
\alias{mi.coef,mi.method-method}
\alias{mi.coef,mi.mixed-method}
\title{ Coefficients from mi.method objecs. }
\description{
  Internal function to extract coefficient values from mi.method objects.
}
\usage{
mi.coef(object)
  \S4method{mi.coef}{mi.method}( object )
  \S4method{mi.coef}{mi.mixed}( object )
}
\arguments{
  \item{object}{ mi.method object class. }
}
\details{
  Extracs the estimated coefficient values from mi.method object family.
}
\value{
  \item{coefficient }{Estimated coefficient.}
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
