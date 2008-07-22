setMethod( "mi.coef", signature( object = "mi.method" ),     
  function ( object ) {
    return( object$model$coefficient )
  }
) 

setMethod( "mi.start", signature( object = "mi.method" ), 
  function( object ){     
    result <- mi.coef( object )
    return ( result )
  }
) 

setMethod( "mi.sigma", signature( object = "mi.method" ),     
  function ( object ) {
    return( object$model$sigma )
  }
)

setMethod( "mi.expected", signature( object = "mi.method" ),     
  function ( object ) {
   return( object$expected )
  }
)
setMethod( "mi.resid", signature( object = "mi.method" ),     
  function ( object, Y ) {
    return( Y - mi.expected ( object ) )
  }
)
setMethod( "mi.imputed", signature( object = "mi.method" ),     
  function ( object, Y ) {
    Y[is.na( Y )] <- object$random
    return( Y )
  }
)
print.mi.method <- function ( x, ... ) {
  cat("model:\n ")
  print(x$model$call)
  cat("\ncoefficients:\n")
  print(x$model$coefficient)
  cat("\nimputed values:\n")
  print(x$random)
}

setMethod( "plot", signature( x = "mi.method", y ="ANY"), 
  function( x, y, main = deparse( substitute( y ) ), gray.scale = FALSE ){      
    fit   <- mi.expected( x )
    res   <- mi.resid( x, y )
    sigma <- mi.sigma( x )
    vrb.obs <- y
    vrb.imp <- mi.imputed( x, y )
    mi.hist(  vrb.obs, object, xlab=main, main = main, gray.scale = gray.scale )
    residual.plot( fit, res, sigma, main = main, gray.scale = gray.scale )
    binnedplot ( fit[ !is.na( y )], res[ !is.na( y )], 
              nclass = sqrt( length( fit[  !is.na( y )] ) ), main = main )
    mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, 
                      main = main, gray.scale = gray.scale )
    #plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
  }
)
