setMethod( "fitted", signature( object = "mi.method" ),     
  function ( object, ... ) {
   return( object$expected )
  }
)

setMethod("fitted", signature(object = "mi.mixed"),     
  function ( object ) {
   return( object$expected )
  }
)
