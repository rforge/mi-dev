\name{typecast}
\docType{genericFunction}
\alias{typecast}
\alias{typecast,ANY-method}
\alias{typecast,matrix-method}
\alias{typecast,data.frame-method}
\title{ Variables type }
\description{ Function for determinig the variable type. }
\usage{
  typecast( object )
  \S4method{typecast}{ANY}( object )
  \S4method{typecast}{matrix}( object )
  \S4method{typecast}{data.frame}( object )
}
\arguments{
  \item{ object }{Vector, matrix, or data frame of data to determin the type of.}
}
\value{ 
  The variable type of a vector, or vector of variable types for each variable in the dataset.
}
\references{
  Andrew Gelman and M. Grazia Pittau, A flexible program for missing-data imputation and model checking, Technical report, Columbia University, New York. 
}

\author{
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}
\seealso{
  \code{\link{mi.models}} 
}
\examples{
data(CHAIN)
class(CHAIN)
## [1] "data.frame"

# for vector
typecast(CHAIN[,1])
## [1] "mixed"

# for matrix
typecast(as.matrix(CHAIN))
##                h39b.W1                  age.W1                  c28.W1 
##                "mixed" "squareroot-continuous" "squareroot-continuous" 
##                 pcs.W1                mcs37.W1                  b05.W1 
##"squareroot-continuous"           "dichotomous"   "ordered-categorical" 
##         haartadhere.W1                 h39b.W2 
##  "ordered-categorical"                 "mixed" 

# for data.frame
typecast(CHAIN)
##                h39b.W1                  age.W1                  c28.W1 
##                "mixed" "squareroot-continuous" "squareroot-continuous" 
##                 pcs.W1                mcs37.W1                  b05.W1 
##"squareroot-continuous"           "dichotomous"   "ordered-categorical" 
##         haartadhere.W1                 h39b.W2 
##  "ordered-categorical"                 "mixed" 
}

\keyword{ models }
