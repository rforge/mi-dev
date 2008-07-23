\name{imputed}
\alias{imputed}
\alias{imputed,mi.method-method}
\alias{imputed,mi.mixed-method}
\title{ Imputed value from mi.method objec class. }
\description{
  Internal function to extract imputed values from mi.method objects.
}
\usage{
imputed(object)
  \S4method{imputed}{mi.method}( object, Y )
  \S4method{imputed}{mi.mixed}( object, Y )
}
\arguments{
  \item{object}{ mi.method or mi.mixed class object. }
  \item{Y}{ Observed values. }
}
\details{
  Extracs the imputed values from mi.method object family.
}
\value{
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
}
\keyword{ models }
