setMethod( "imputed", signature( object = "mi.method" ),     
  function ( object, Y ) {
    Y[is.na( Y )] <- object$random
    return( Y )
  }
)
setMethod("imputed", signature(object = "mi.mixed"),     
  function ( object, Y ) {
    Y[ is.na( Y ) ] <- object$random
    return( Y )
  }
)
