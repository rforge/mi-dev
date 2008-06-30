\name{mi.fixed}
\docType{class}
% Classes
\alias{mi.fixed-class}
% Function
\alias{mi.fixed}
\title{
    Elementary function: imputation of constant variable.
}
\description{
    Imputes univariate constant missing data.
}

\usage{
    mi.fixed( Y , ...)
}

\arguments{
    \item{Y}{ Incomplete data vector of length n. Missing values are coded as NA's. }
    \item{\dots}{ Currently not used. }
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
    \code{ \link{mi.expected}},
    \code{ \link{mi.resid}},
    \code{ \link{mi.sigma}},
    \code{ \link{mi.coef}},
    \code{ \link{mi.info}}, 
    \code{ \link{mi}}
}


\keyword{ models }
