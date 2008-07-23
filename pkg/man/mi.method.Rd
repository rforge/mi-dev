\name{mi.method}
\docType{class}
% Classes
\alias{mi.method-class}
% Function
\alias{imputed}
\alias{print,mi.method-method}
\alias{plot,mi.method,ANY-method}
\alias{fitted,mi.method-method}
\alias{coef,mi.method-method}
\alias{imputed,mi.method-method}
\alias{resid,mi.method-method}
\alias{sigma.hat,mi.method-method}

\title{
    Virtual class for all mi. classes.
}
\description{
    Imputes univariate missing data using linear regression.
}
\usage{
  \S4method{imputed}{mi.method}(object,y)
  \S4method{coef}{mi.method}(object)
  \S4method{sigma.hat}{mi.method}(object)
  \S4method{fitted}{mi.method}(object)
  \S4method{resid}{mi.method}(object, y)
  \S4method{print}{mi.method}(x, \dots)
  \S4method{plot}{mi.method,ANY}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{mi.method} object.}
  \item{\dots}{ Currently not used. }
  \item{x}{ \code{mi.method} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }

}
\details{

}
\value{
%    \item{model   }{A summary of the fitted model.}
%    \item{expected}{The expected values estimated by the model.}
%    \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution.} 
}
\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2006.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\examples{

}

\seealso{
    \code{\link{mi}}
}

\keyword{ models }
