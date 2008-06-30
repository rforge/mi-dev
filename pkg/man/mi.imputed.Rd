\name{mi.imputed}
\alias{mi.imputed}
\alias{mi.imputed,mi.method-method}
\alias{mi.imputed,mi.mixed-method}
\title{ Imputed value from mi.method objec class. }
\description{
  Internal function to extract imputed values from mi.method objects.
}
\usage{
mi.imputed(object,\dots)
  \S4method{mi.imputed}{mi.method}( object )
  \S4method{mi.imputed}{mi.mixed}( object )
}
\arguments{
  \item{object}{ mi.method object class. }
  \item{\dots}{ currently not used. }
}
\details{
  Extracs the imputed values from mi.method object family.
}
\value{
  \item{imputed}{imputed value}
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
