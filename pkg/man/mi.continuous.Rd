\name{mi.continuous}
\docType{class}
% Classes
\alias{mi.continuous-class}
% Function
\alias{mi.continuous}
\title{
    Elementary function: linear regression to impute a continuous variable.
}
\description{
    Imputes univariate missing data using linear regression.
}
\usage{
mi.continuous(formula, data = NULL, start = NULL, n.iter = 100, draw.from.beta = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{\dots}{ Currently not used. }
}
\details{
    see bayesglm
}
\value{
    \item{model   }{A summary of the fitted model.}
    \item{expected}{The expected values estimated by the model.}
    \item{random  }{Vector of length n.mis of random predicted values predicted by using the normal distribution.} 
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

\seealso{
    \code{\link{mi.expected}},
    \code{\link{mi.resid}},
    \code{\link{mi.sigma}},
    \code{\link{mi.coef}}, 
    \code{\link{mi.info}}, 
    \code{\link{mi}}
}


\keyword{ models }