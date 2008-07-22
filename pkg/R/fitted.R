# ==============================================================================
# extract fitted values for mi.method class object
# ==============================================================================
setMethod( "fitted", signature( object = "mi.method" ),     
  function ( object, ... ) {
    return( object$expected )
  }
)
# ==============================================================================
# extract fitted values for mi.mixed class object
# ==============================================================================
setMethod("fitted", signature(object = "mi.mixed"),     
  function ( object ) {
    return( object$expected )
  }
)
