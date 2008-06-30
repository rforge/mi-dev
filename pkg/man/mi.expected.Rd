\name{mi.expected}
\alias{mi.expected}
\alias{mi.expected,mi.method-method}
\alias{mi.expected,mi.mixed-method}
\title{ Expected value from mi.method objec class. }
\description{
  Internal function to extract expected values from mi.method objects.
}
\usage{
mi.expected(object,\dots)
  \S4method{mi.expected}{mi.method}( object )
  \S4method{mi.expected}{mi.mixed}( object )
}
\arguments{
  \item{object}{ mi.method object class. }
  \item{\dots}{ currently not used. }
}
\details{
  Extracs the expected values from mi.method object family.
}
\value{
  \item{expected}{expected value}
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
