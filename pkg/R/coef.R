# ==============================================================================
# extract coefficient values for mi.method class object
# ==============================================================================

coef.mi.method <- function(object, ...){
  object$model$coefficient
}
  
#
#setMethod( "coef", signature( object = "mi.method" ),     
#  function ( object, ... ) {
#    return( object$model$coefficient )
#  }
#) 

#coef.mi.logcontinuous <- function(object, ...){
#  return( list( object$model$model.1$coefficient, 
#                object$model$model.2$coefficient ) )
#}
#
#
#
