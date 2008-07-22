# ==============================================================================
# extract imputed values for mi.method class object
# ==============================================================================
setMethod( "imputed", signature( object = "mi.method" ),     
  function ( object, Y ) {
    Y[is.na( Y )] <- object$random
    return( Y )
  }
)
# ==============================================================================
# extract imputed values for mi.mixed class object
# ==============================================================================
setMethod("imputed", signature(object = "mi.mixed"),     
  function ( object, Y ) {
    Y[ is.na( Y ) ] <- object$random
    return( Y )
  }
)
