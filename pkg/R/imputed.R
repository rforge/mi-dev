# ==============================================================================
# extract imputed values for mi.method class object
# ==============================================================================
setMethod( "imputed", signature( object = "mi.method" ),     
  function ( object, y ) {
    y[is.na( y )] <- object@random
    return( y )
  }
)


setMethod( "imputed", signature( object = "mi.categorical" ),     
  function ( object, y ) {
    y.level <- levels(y)
    y.imp <- y.level[object@random]
    y[is.na( y )] <- y.imp
    return( y )
  }
)
