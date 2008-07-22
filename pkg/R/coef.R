# ==============================================================================
# extract coefficient values for mi.method class object
# ==============================================================================
setMethod( "coef", signature( object = "mi.method" ),     
  function ( object, ... ) {
    return( object$model$coefficient )
  }
) 
# ==============================================================================
# extract coefficient values for mi.mixed class object
# ==============================================================================
setMethod("coef", signature(object = "mi.mixed"),     
  function ( object ) {
   result <- list(object$model$model.1$coefficient,object$model$model.2$coefficient)
    return( result )
  }
)
