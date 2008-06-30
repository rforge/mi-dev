# ==============================================================================
# functions for mi.method class
# ==============================================================================

setMethod("mi.coef", signature(object = "mi.method"),     
  function ( object ) {
    return( object$model$coefficient )
  }
)
setMethod("mi.sigma", signature(object = "mi.method"),     
  function ( object ) {
    return( object$model$sigma )
  }
)
setMethod("mi.expected", signature(object = "mi.method"),     
  function ( object ) {
   return( object$expected )
  }
)
setMethod("mi.resid", signature(object = "mi.method"),     
  function ( object, Y ) {
    return( Y - mi.expected ( object ) )
  }
)
setMethod("mi.imputed", signature(object = "mi.method"),     
  function ( object, Y ) {
    Y[is.na(Y)]<-object$random
    return(Y)
  }
)
print.mi.method <- function ( object ) {
  cat("model:\n ")
  print(object$model$call)
  cat("\ncoefficients:\n")
  print(object$model$coefficient)
  cat("\nimputed values:\n")
  print(object$random)
}

#setMethod("mi.start", signature(object = "mi.method"), 
#  function( object ){
#    result <- list( coefficient = NULL )
#    result$coefficient <- paste(",start =c(", paste(mi.coef(object),collapse=", "),")" )       
#    return ( result )
#  }
#)
