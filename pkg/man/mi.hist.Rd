\name{mi.hist}
\docType{genericFunction}
\alias{mi.hist}
\alias{mi.hist,ANY,ANY-method}
\alias{mi.hist,ANY,mi.method-method}
\alias{mi.hist,ANY,mi.categorical-method}
\alias{mi.hist,ANY,mi.dichotomous-method}
\alias{mi.hist,ANY,mi.mixed-method}
\alias{mi.hist,ANY,mi.polr-method}
\alias{mi.hist,ANY,mi.sqrtcontinuous-method}
\alias{mi.hist,ANY,mi.pmm-method}
\title{ 
    Multiple Imputation Histogram 
}
\description{
    A function for plotting the histogram of each variable and of its observed and imputed values. 
}
\usage{
  mi.hist( Yobs, object, \dots)
\S4method{mi.hist}{ANY,mi.method}( Yobs, object, ...)
\S4method{mi.hist}{ANY,mi.categorical}( Yobs, object, ...)
\S4method{mi.hist}{ANY,mi.dichotomous}( Yobs, object, ...)
\S4method{mi.hist}{ANY,mi.mixed}( Yobs, object, ...)
\S4method{mi.hist}{ANY,mi.polr}( Yobs, object, ...)
\S4method{mi.hist}{ANY,mi.sqrtcontinuous}( Yobs, object, ...)
\S4method{mi.hist}{ANY,mi.pmm}( Yobs, object, ...)
}
\arguments{
    \item{Yobs}{ observed values.}
    \item{object}{ imputed values or member object of mi.method object family. }
    \item{\dots}{ Other options for plot function. }  
}
\value{
    The histogram (in black) of the complete variable, the histogram (in blue) of the observed values
    and the histogram (in red) of the imputed values. 
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
    The histogram of the completed values (observed plus imputed) is in black, 
    the histogram of the imputed values in red, 
    while the one of the observed values in blue.}
\seealso{
    \code{\link{plot.mi}}, 
    \code{\link{plot}},
    \code{\link{hist}}
}
\examples{
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  # create artificial missingness on y
  y[seq(2,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  dat.cont.mi <- mi.continuous(y~x, data = dat.xy)
  mi.hist( y, dat.cont.mi)

  # imputation
  dat.mi <- mi(dat.xy)
  mi.hist( y, imp(dat.mi,1)[["y"]])
}
\keyword{ models }
