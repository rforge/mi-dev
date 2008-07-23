\name{mi.dichotomous}
\docType{class}
% Classes
\alias{mi.dichotomous-class}
% Function
\alias{mi.dichotomous}
\alias{plot,mi.dichotomous-method}
% methods
\alias{resid,mi.dichotomous-method}
\title{
    Elementary function: Bayesian logistic regression to impute a dichotomous variable.
}
\description{
    Imputes univariate missing data using bayesglm, an 
    R functions for generalized linear modeling with independent normal, t, or
    Cauchy prior distribution for the coefficients.
}
\usage{
mi.dichotomous(formula, data = NULL, start = NULL, n.iter = 100, draw.from.beta = FALSE, ...)

\S4method{resid}{mi.dichotomous}(object, Y)
  \S4method{plot}{mi.dichotomous}( x, y, \dots )
}

\arguments{
  \item{formula}{  an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details. }
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{\dots}{ Currently not used. }
  \item{object}{\code{mi} object.}
  \item{Y}{observed values}
    \item{x}{ An object of class '"mi.dichotomous"'.}
  \item{y}{ Observed values.}
}
\details{
    In bayesglm default the prior distribution is Cauchy with center 0 and scale 2.5 
    for all coefficients (except for the intercept, which has a prior scale of 10). 
    See also glm for other details.
}
\value{
    \item{model    }{A summary of the bayesian fitted model.}
    \item{expected }{The expected values estimated by the model.}
    \item{random   }{Vector of length n.mis of random predicted values predicted by using the binomial distribution.}
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

\note{ see also http://www.stat.columbia.edu/~gelman/standardize/ }

\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rbinom(100,1,invlogit(1+2*x)) # y ~ Bin(1,invlogit(1 + 2*x)
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  mi.dichotomous(y~x, data = dat.xy)
}
\seealso{
    \code{\link{mi.info}}, 
    \code{\link{mi}}
}
\keyword{ models }
