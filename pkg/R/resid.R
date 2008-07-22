# ==============================================================================
# extract residual values for mi.method class object
# ==============================================================================
setMethod( "resid", signature( object = "mi.method" ),     
  function ( object, Y ) {
    return( Y - fitted( object ) )
  }
)
# ==============================================================================
# extract residual values for mi.dichotomous class object
# ==============================================================================
setMethod( "resid", signature( object = "mi.dichotomous" ),     
  function ( object, Y ) {
    return( dicot( Y ) - fitted ( object ) )
  }
)
# ==============================================================================
# extract residual values for mi.logcontinuous class object
# ==============================================================================
setMethod( "resid", signature( object = "mi.logcontinuous" ),     
  function ( object, Y ) {
    return( log( Y ) - log( fitted ( object ) ) )
  }
)
# ==============================================================================
# extract residual values for mi.sqrtcontinuous class object
# ==============================================================================
setMethod( "resid", signature( object = "mi.sqrtcontinuous" ),     
  function ( object, Y ) {
    #return( sqrt(Y) - sqrt(mi.expected ( object )) )
    return( Y - fitted ( object ) )
  }
)
# ==============================================================================
# extract residual values for mi.mixed class object
# ==============================================================================
setMethod( "resid", signature( object = "mi.mixed" ),     
  function ( object, Y ) {
   return( list(residual.values.1 = 1*( Y > 0 ) - fitted( object )[[1]], 
                residual.values.2 = Y[as.double( names( fitted( object )[[2]]))]-fitted( object )[[2]]) )
  }
)
