\name{residual.plot}
\alias{residual.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ residual plot for the observed values}
\description{
  Plots the residual of observed variable.
}
\usage{
residual.plot(Expected, Residuals, sigma, main = deparse(substitute(Expected)), col.pts = "blue", col.ctr = "red", col.sgm = "black", cex = 0.5, gray.scale = FALSE, xlab = "Predicted", ylab = "Residuals", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Expected}{ Expected value. }
  \item{Residuals}{ Residual value. }
  \item{sigma}{ Standard error. }
  \item{main}{ main for the plot.  See plot for detail.}
  \item{col.pts}{ Color of the points. }
  \item{col.ctr}{ Color of the line at zero. }
  \item{col.sgm}{ Color of standard error line. }
  \item{cex}{ A numerical value giving the amount by which plotting text
          and symbols should be magnified relative to the default.  See par for detail. }
  \item{gray.scale}{ If TRUE, makes the plot into black and white.  This option overwrites the color specification.  Default is FALSE. }
  \item{xlab}{ Label for x axis. }
  \item{ylab}{ Label for y axis. }
  \item{\dots}{ Additional parameters passed to 'plot' function. }
}
\value{
  Plot to visualize pattern of residulal value for the expected value.
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

\examples{

  # In regression context
  # data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  lm.fit <- lm(y~x)
  residual.plot (lm.fit$fitted.values, lm.fit$residuals, sigma.hat(lm.fit))

  # In missing data imputation context ( residual of conditional model)
  # true data
  x<-rnorm(100,0,1) # N(0,1)
  y<-rnorm(100,(1+2*x),1.2) # y ~ 1 + 2*x + N(0,1.2)
  # create artificial missingness on y
  y[seq(1,100,10)]<-NA
  dat.xy <- data.frame(x,y)
  # imputation
  dat.cont.mi <- mi.continuous(y~x, data = dat.xy)
  residual.plot(expected(dat.cont.mi), resid(dat.cont.mi,y), sigma.hat(dat.cont.mi))

}

\keyword{ hplot }
