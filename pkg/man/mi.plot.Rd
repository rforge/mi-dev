\name{mi.plot}
\alias{mi.plot}
\alias{mi.plot,mi.method-method}
\alias{mi.plot,mi.categorical-method}
\alias{mi.plot,mi.dichotomous-method}
\alias{mi.plot,mi.logcontinuous-method}
\alias{mi.plot,mi.mixed-method}
\alias{mi.plot,mi.polr-method}
\alias{mi.plot,mi.sqrtcontinuous-method}
\alias{mi.plot,mi.copy-method}

\title{ Default plotting function for mi.method objects. }
\description{
  Plotting function for mi.method object class.
}
\usage{
mi.plot(object, Yobs, \dots)
  \S4method{mi.plot}{mi.method}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.mixed}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.dichotomous}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.categorical}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.logcontinuous}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.polr}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.sqrtcontinuous}( object, Yobs, \dots )
  \S4method{mi.plot}{mi.copy}( object, Yobs, \dots )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ mi.method object family }
  \item{Yobs}{ Observed values. }
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
