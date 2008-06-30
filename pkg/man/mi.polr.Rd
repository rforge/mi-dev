\name{mi.polr}
\docType{class}
% Classes
\alias{mi.polr-class}
% Function
\alias{mi.polr}
\title{
    Elementary function: multinomial log-linear models to impute a ordered categorical variable.
}
\description{
    Imputes missing data in a categorical variable using multinomial Log-linear Models. 
}
\usage{
mi.polr(formula, data = NULL, drop.unused.levels = TRUE, start = NULL, n.iter = 100, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ a formula expression as for regression models, of the form
          'response ~ predictors'. The response should be a factor
          (preferably an ordered factor), which will be interpreted as
          an ordinal response, with levels ordered as in the factor.  
          The model must have an intercept: attempts to remove one will
          lead to a warning and be ignored.  An offset may be used. 
          See the documentation of 'formula' for other details. }
  \item{data}{ A data frame containing the incomplete data and the matrix of the complete predictors. }
  \item{drop.unused.levels}{ Drops unused levels. }
  \item{start}{ Starting value for bayespolr. }
  \item{n.iter}{ Maximum number of iteration for bayespolr. The default is 100.}
  \item{\dots}{ Currently not used. }
}
\details{
    'multinom' calls the library 'nnet'. See multinom for other details.   
}
\value{
    \item{model    }{A summary of the multinomial fitted model}
    \item{expected }{The expected values estimated by the model}
    \item{random   }{Vector of length n.mis of random predicted values predicted by using the multinomial distribution}
    \item{residual }{The residual vector of length same as Y}
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
    \code{\link{mi.models}}, 
    \code{\link{mi}}
}
\keyword{ models }
