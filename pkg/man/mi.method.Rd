\name{mi.method}
\docType{class}
% Classes
\alias{mi.method-class}
% Function
\alias{mi.}
\alias{print,mi.method-method}
\alias{plot,mi.method-method}
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
mi.continuous(formula, data = NULL, start = NULL, n.iter = 100, draw.from.beta = FALSE, ...)
  \S4method{imputed}{mi.method}(object)
  \S4method{coef}{mi.method}(object)
  \S4method{sigma.hat}{mi.method}(object)
  \S4method{fitted}{mi.method}(object)
  \S4method{resid}{mi.method}(object, Y)
  \S4method{print}{mi.method}(x, \dots)
  \S4method{plot}{mi.method}( x, y, \dots )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
%  \item{formula}{an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
%  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
%  \item{start}{ Starting value for bayesglm. }
%  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
%  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
%  \item{\dots}{ Currently not used. }
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
