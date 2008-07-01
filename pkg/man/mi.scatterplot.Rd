\name{mi.scatterplot}
\alias{mi.scatterplot}
\title{ Multiple Imputation Scatterplot }
\description{
  A function for plotting, for each variable its observed and imputed values. 
}
\usage{
mi.scatterplot( Yobs, Yimp, X = NULL, xlab = NULL, ylab = NULL, 
                            main = "Imputed Variable Scatter Plot", 
                             display.zero = TRUE, gray.scale = FALSE, 
                              obs.col = rgb( 0, 0, 1, alpha = 0.3 ), 
                              imp.col = rgb( 1, 0, 0 ), 
                              obs.pch = 20 , imp.pch = 20, 
                              obs.cex = 0.3, imp.cex = 0.3, 
                              obs.lty = 1  , imp.lty = 1, 
                              obs.lwd = 2.5, imp.lwd = 2.5, ... )
} 
\arguments{ 
  \item{Yobs}{ observed values. }
  \item{Yimp}{ imputed values. }
  \item{X}{ variable to plot on the x axis. }
  \item{xlab}{ label on the x axis. }
  \item{ylab}{ label on the y axis. }
  \item{display.zero}{ if set to FALSE zeros will not be displayed. Default is TRUE.}
  \item{main}{ main title of the plot. }
  \item{gray.scale}{ When set to TRUE, makes the plot into gray scale with predefined color and line type. }
  \item{obs.col}{ color for the observed variable.  Default is "blue". }
  \item{imp.col}{ color for the imputed variable.  Default is "red". }
  \item{obs.pch}{ data symbol for observed variable.  Default is 20. }
  \item{imp.pch}{ data symbol for imputed variable.  Default is 20. }
  \item{obs.cex}{ text size for observed variable. Default is 0.3. }
  \item{imp.cex}{ text size for imputed variable. Default is 0.3. }
  \item{obs.lty}{ line type for observed variable. Default is 1. }
  \item{imp.lty}{ line type for imputed variable. Default is 1. }
  \item{obs.lwd}{ line width for observed variable. Default is 2.5. }
  \item{imp.lwd}{ line width for imputed variable. Default is 2.5. }
  \item{\dots}{ Other options for 'plot' function. }
}
\details{
 Since several data points can have the same data values, we added a small random number to each so that they do not fall on top of each other. This is called jittering.  Fitted lowess lines are also added for both the imputed and observed data.
}
\value{
  A scatterplot with the observed (in blues) and the imputed values (in red) plotted versus a chosen variable. 
}
\references{ 
  Andrew Gelman and Jennifer Hill, 
  Data Analysis Using Regression and Multilevel/Hierarchical Models, 
  Cambridge University Press, 2006. }

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}}
\note{ 
  By default imputed values are in red, while the observed values are in blue.
}
\seealso{\code{\link{plot.mi}}, \code{\link{plot}}}
\keyword{ hplot }
