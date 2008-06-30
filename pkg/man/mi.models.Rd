\name{mi.models}
\alias{mi.models}
\title{Multiple imputation models}
\description{A function that selects the imputation models according to the variables type 
 and produces a text file as an output.}
\usage{
    mi.models( data, filename = NULL, type.list = NULL, model.print = TRUE ,
                model.function = imputation.models, factor.type = "unordered-categorical" ) 
}

\arguments{
  \item{data}{ A data frame containing the incomplete data. Missing data are coded as NA's.}
  \item{type.list}{ Type of the variables for all of the colums of "data".} 
  \item{filename}{ The name of the text file which the models are to be written to. By default is NULL, no text file is saved.}
  \item{model.print}{ Logical flag (TRUE=to print the models).}
  \item{model.function}{ Specify function to get the default model. default is imputation.models }
  \item{factor.type}{ Specification of type of variable to be factored. }
}

\details{
    This function enables the user to modify the models for imputing the NA's, for example modifying or elinating 
    inputs in the imputation model.
    To run:
        \enumerate{
        \item Run \code{mi.model} that writes the model in a ASCII file. 
        \item Edit the text file for example by selecting only some imputs.
        \item Save the tex file.
        \item Run the \code{mi} function with the name of the text file (see Example).
    }
}
\value{
  List of the imputation models of lenght equal to the number of variables with missing data.
  The models list is also written in a text file.
}
\references{
    Andrew Gelman and M. Grazia Pittau,  
    A flexible program for missing-data imputation and model checking, 
    Technical report, Columbia University, New York;
    Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.}

\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}

\note{ 
    The file will be created into the working directory. See  \code{getwd()} and \code{setwd()} for detail.
    Once the text file is modifed to read it in R has to be an argument of mi function. 
}

\seealso{ \code{ \link{mi} } }
\examples{
data(CHAIN)
models <- mi.models(CHAIN, filename=NULL)  ##print the list of the imputation models
models[[1]]  ##the first element of the list

##names(CHAIN)
 #[1] "h39b.W1"        "age.W1"         "c28.W1"         "pcs.W1"         "mcs37.W1"      
 #[6] "b05.W1"         "haartadhere.W1" "h39b.W2"

models$h39b.W1  ##The list of the two models for the "mixed" variable h39b.W1
models$age.W1   ##The model for the variable age

#============================================================
# Writing models in models.txt
#============================================================
models <- mi.models(CHAIN, filename="models.txt", model.print = TRUE) 

## the file:
file.show("imp.models.txt")

#============================================================
#Specifying the incomplete columns
#============================================================

# random sample of size 100
samp <- c(1, 3, 4, 8, 11, 12, 13, 14, 17, 18, 19, 20, 27, 28, 31,
          38, 42, 46, 47, 48, 49, 52, 53, 54, 55, 57, 62, 63, 64, 65,
          67, 68, 69, 70, 72, 73, 78, 79, 83, 84, 85, 87, 91, 92, 94,
          99, 100, 106, 107, 108, 111, 112, 113, 115, 118,
          124, 125, 126, 129, 131, 133, 136, 139, 142, 143, 145, 147,
          152, 153, 156, 159, 160, 161, 164, 165, 166, 168, 169, 171,
          172, 173, 174, 175, 177, 178, 181, 182, 185, 189, 192,
          195, 197, 203, 205, 211, 212, 214, 368, 372, 416) 

CHAIN.samp <- CHAIN[samp,]
models <- mi.models(CHAIN.samp)
#$h39b.W1
#$h39b.W1$mod.1
#[1] "impute.1.h39b.W1 = bayesglm(dicot( h39b.W1 >0 ) ~ c28.W1 + b05.W1 + h39b.W2 + age.W1 + 
#       pcs.W1 + mcs37.W1 + haartadhere.W1 , family=binomial(link ='logit'))"

#$h39b.W1$mod.2
#[1] "impute.2.h39b.W1 = lm(I( h39b.W1 ) ~ c28.W1 + b05.W1 + h39b.W2 + age.W1 + pcs.W1 + mcs37.W1 +
#      haartadhere.W1 , subset= h39b.W1 >0)"


#$c28.W1
#[1] "impute.c28.W1 = multinom( c28.W1 ~ h39b.W1 + b05.W1 + h39b.W2 + age.W1 + pcs.W1 + mcs37.W1 + 
#      haartadhere.W1 , maxit = 200, trace = FALSE)"

#$b05.W1
#[1] "impute.b05.W1 = multinom( b05.W1 ~ h39b.W1 + c28.W1 + h39b.W2 + age.W1 + pcs.W1 + mcs37.W1 + 
#      haartadhere.W1 , maxit = 200, trace = FALSE)"

#$h39b.W2
#$h39b.W2$mod.1
#[1] "impute.1.h39b.W2 = bayesglm(dicot( h39b.W2 >0 ) ~ h39b.W1 + c28.W1 + b05.W1 + age.W1 + pcs.W1 + 
#      mcs37.W1 + haartadhere.W1 , family=binomial(link ='logit'))"

#$h39b.W2$mod.2
#[1] "impute.2.h39b.W2 = lm(I( h39b.W2 ) ~ h39b.W1 + c28.W1 + b05.W1 + age.W1 + pcs.W1 + mcs37.W1 + 
#     haartadhere.W1 , subset= h39b.W2 >0)"
}
\keyword{ models }

