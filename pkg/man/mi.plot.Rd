\name{plot}
\alias{plot}
\alias{plot,mi.categorical-method}
\alias{plot,mi.dichotomous-method}
\alias{plot,mi.logcontinuous-method}
\alias{plot,mi.polr-method}
\alias{plot,mi.sqrtcontinuous-method}
\alias{plot,mi.copy-method}


\title{ Default plotting function for mi.method xs. }
\description{
  Plotting function for mi.method x class.
}
\usage{
mi.plot(x, y, \dots)
  \S4method{plot}{mi.method}( x, y, \dots )
  \S4method{plot}{mi.mixed}( x, y, \dots )
  \S4method{plot}{mi.dichotomous}( x, y, \dots )
  \S4method{plot}{mi.categorical}( x, y, \dots )
  \S4method{plot}{mi.logcontinuous}( x, y, \dots )
  \S4method{plot}{mi.polr}( x, y, \dots )
  \S4method{plot}{mi.sqrtcontinuous}( x, y, \dots )
  \S4method{plot}{mi.copy}( x, y, \dots )
}
%- maybe also 'usage' for other xs documented here.
\arguments{
  \item{x}{ mi.method x family }
  \item{y}{ Observed values. }
  \item{\dots}{ Currently not used.}
}
\details{
  Default plotting function for mi.method class objects.
}

\value{
  Histograms, scatterplots, and residual plots of the fit of the imputation models.  
  Binned residual plots are for each dichotomous variable.
}
\references{
    Andrew Gelman and M. Grazia Pittau,  
    A flexible program for missing-data imputation and model checking, 
    Technical report, Columbia University, New York; Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\seealso{
  \code{\link{mi}}, 
  \code{\link{mi.scatterplot}},
  \code{\link{mi.hist}}
}
\keyword{ hplot }
