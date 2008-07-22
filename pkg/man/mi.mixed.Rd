\name{mi.mixed}
\docType{class}
% Classes
\alias{mi.mixed-class}
% Function
\alias{mi.mixed}
\alias{coef,mi.mixed-method}
\title{
    Two-stage elementary function: linear regression to impute a variable that can be positive or zero. }
\description{
    Imputes univariate missing data using a two stage imputation process. 
    The first regression model is fit to all the data for which the variable is observed, 
    and the second model is fit to all the data for which the variable is observed and positive.
}
\usage{
mi.mixed(formula, data = NULL, start = NULL, n.iter = 100, draw.from.beta = FALSE, ...)

\S4method{coef}{mi.mixed}(object)
\S4method{sigma.hat}{mi.mixed}(object)
\S4method{fitted}{mi.mixed}(object)
\S4method{resid}{mi.mixed}(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ an object of class '"formula"' (or one that can be coerced to that class): a symbolic description of the model to be fitted.  See bayesglm 'formula' for details.}
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{start}{ Starting value for bayesglm. }
  \item{n.iter}{ Maximum number of iteration for bayesglm. The default is 100.}
  \item{draw.from.beta}{ Draws from posterior distribution of the betas to add randomness. }
  \item{\dots}{ Currently not used. }
}
\details{
    Imputation variable that can be positive or zero. 
    See Gelman and Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, (2206), Chapter 25. 
}
\value{
    \item{model$model.1}{A summary of the fitted model for the all the data for which y is observed.}
    \item{model$model.2}{A summary of the fitted model for the all the data for which y is observed and positive.}
    \item{expected}{The expected values estimated by the model}
    \item{random }{Vector of length n.mis of random predicted values to impute the missing data. 
                    Whether missing values are positive random is positive, otherwise is equal to zero.}
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
\seealso{
    \code{\link{mi.expected}},
    \code{\link{mi.info}},  
    \code{\link{mi}}
}
\keyword{ models }
