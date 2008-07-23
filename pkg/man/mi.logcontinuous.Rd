\name{mi.logcontinuous}
\docType{class}
% Classes
\alias{mi.logcontinuous-class}
% Function
\alias{mi.logcontinuous}
\alias{resid,mi.logcontinuous-method}
\alias{plot,mi.logcontinuous,ANY-method}

\title{
    Elementary function: linear regression to impute a continuous positive variable.
}
\description{
    Imputes univariate missing data using linear regression on the logarithm values.
}
\usage{
  mi.logcontinuous(formula, data = data.frame(cbind(Y, X)), start = NULL, n.iter = 100, ... )
  \S4method{resid}{mi.logcontinuous}(object, y)
  \S4method{plot}{mi.logcontinuous,ANY}( x, y, main=deparse( substitute( y ) ), gray.scale = FALSE )
}

\arguments{
  \item{formula}{an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{\dots}{ Currently not used. }
  \item{object}{\code{mi.logcontinuous} object.}
  \item{x}{\code{mi.logcontinuous} object.}
  \item{y}{ Observed values.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }
}

\details{
    see \code{\link[stats]{lm}}.
}
\value{
  
  \item{model   }{A summary of the fitted model}
  \item{expected}{The expected values estimated by the model}
  \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution} 
}

\references{
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
        
\note{ 
    The incomplete data vector has to be positive.
}
\seealso{
    \code{\link{mi.info}},  
    \code{\link{mi}}
}
\keyword{ models }
