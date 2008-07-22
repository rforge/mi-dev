setMethod( "sigma.hat", signature( object = "mi.method" ),     
  function ( object,... ) {
    return( object$model$sigma )
  }
)

setMethod("sigma.hat", signature(object = "mi.mixed"),     
  function ( object,... ) {
    return( object$model$model.2$sigma )
  }
)
