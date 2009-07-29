\name{noise.control}
\alias{noise.control}

\title{Auxiliary for Adding Priors to Missing Data Imputation}
\description{
  Auxiliary function as user interface for adding noise for \code{mi} procedure.
  Typically only used when calling \code{mi}.
}

\usage{
noise.control(method=c("reshuffling", "fading"), pct.aug=10, K=1)

}
\arguments{
  \item{method}{two methods are implemented: \code{reshuffling} and \code{fading}.}  
  \item{pct.aug}{percent of N being add into the exisitng data, where N is the number of 
    observation of the completed data set, default is 10.}  
  \item{K}{the cooling parameter, default is 1.}
}

\details{
If \code{reshuffling} method is used, the imputation will randomly switch between 
randomly imputing data from marginal (imputing data from the observed values)
and drawing from the modeled based values. And with each iteration (s), the probability of cooling (q) decreases by number of iterations, 
such that \code{q = K/s}.

If \code{fading} method is used, the imputation will augment 
}

\references{
    Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajimi. \emph{Forthcoming}.
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software}.

}


\author{
  Yu-Sung Su \email{yajima@stat.columbia.edu}
}

\seealso{ 
    \code{\link{mi}} 
}

\examples{
### NOT RUN
#===========================================================
# add fadding empirical noise by augmenting 10% of the data
#===========================================================
if(0){
 data(CHAIN)
 IMP <- mi(CHAIN, add.noise=noise.control(method="fading", pct.aug=10))
#=================================================
# add noise by randomly drawing from the marginal
#=================================================
## This is the default setting
 IMP <- mi(CHAIN, add.noise=noise.control(method="reshuffling", K=1))
#=================================================
# add no noise
#=================================================
 IMP <- mi(CHAIN, add.noise=FALSE)
}
}

\keyword{manip}