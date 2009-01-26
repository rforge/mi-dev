# ==============================================================================
# extract sigma.hat values for mi.method class object
# ==============================================================================
setMethod( "sigma.hat", signature( object = "mi.method" ),     
  function ( object, ... ) {
    return( object$model$sigma )
  }
)
