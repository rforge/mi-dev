# ==============================================================================
# method definition 
# ==============================================================================

setMethod( "mi.start", signature( object = "mi.method" ), 
  function( object ){     
    result <- mi.coef( object )
    return ( result )
  }
) 

setMethod( "mi.coef", signature( object = "mi.method" ),     
  function ( object ) {
    return( object$model$coefficient )
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

setMethod( "show", signature( object = "mi" ),
  function ( object ) {
    print( object )
  }
) 

setMethod("show", signature( object = "mi.info" ), 
  function ( object ) {
    print( object )
  }
)

setMethod( "mi.hist", signature( object = "mi.method" ),     
  function (  Yobs, object,  b = NULL, binwidth = NULL, gray.scale = FALSE, 
              main = paste("Histogram of ", deparse( substitute( Yobs ) )),  
              xlab = deparse( substitute( Yobs ) ), ylab = "Frequency", 
              obs.col = "blue", imp.col = "black", mis.col = "red",
              obs.lty = 1, imp.lty = 1, mis.lty = 1,
              obs.lwd = 1, imp.lwd = 1, mis.lwd = 1, mlt = 0.1, ... )
{
  Yimp <-mi.imputed( object, Yobs )
  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if( is.null( binwidth ) ) { 
    binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )
  }
  if( is.null( b )) { 
    b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )
  }
  if( gray.scale == TRUE ) { 
    obs.col = gray( 0.6 ) 
    imp.col = gray( 0.8 ) 
    mis.col = gray( 0 )
    obs.lty = 3
    imp.lty = 1
    mis.lty = 1
  }
  h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
  h.mis <- hist( mis,  plot = FALSE, breaks = b )
  h.imp <- hist( Yimp, plot = FALSE, breaks = b )
  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) * 1.05 ), 
        yaxs = "i", xlab = xlab,xaxt = "n",xlim = range( Yimp ),ylab = ylab, 
        type = "n", bty = "l", main = main )
  axis( 1, tick = TRUE, col.axis = 'black' )
  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) { mlt<-0.2 }
  histlineplot ( h.mis, shift = -mlt*binwidth, 
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
  histlineplot ( h.obs, shift = mlt*binwidth, 
                  col = obs.col, lty = obs.lty, lwd = obs.lwd ) 
  histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )  

}
)
##The function for the histogram
histlineplot <- function ( h, shift = 0, col = "black", zero = TRUE, 
                            lty = 1, lwd = 1, ... ) {
  n.bins <- length ( h$breaks ) - 1
  x.pos  <- h$breaks[ rep( c( 1, 2:n.bins, n.bins+1 ), c( 1, rep( 2, n.bins-1 ), 1 ) ) ]
  y.pos  <- rep ( h$counts, rep( 2, n.bins ) )
  if ( zero ) {
    x.pos <- c( x.pos[1], x.pos, x.pos[ length( x.pos ) ] )
    y.pos <- c( 0, y.pos, 0 )
  }
  lines ( x.pos + shift, y.pos, col = col, lty = lty, lwd = lwd )
}

setMethod( "mi.plot", signature( object = "mi.method" ), 
  function( object, Yobs, main = deparse( substitute( Yobs ) ), gray.scale = FALSE ){      
    fit   <-mi.expected( object )
    res   <- mi.resid( object, Yobs )
    sigma <- mi.sigma( object )
    vrb.obs <- Yobs
    vrb.imp <- mi.imputed( object, Yobs )
    mi.hist(  vrb.obs, object, xlab=main, main = main, gray.scale = gray.scale )
    residual.plot( fit, res, sigma, main = main, gray.scale = gray.scale )
    binnedplot ( fit[ !is.na( Yobs )], res[ !is.na( Yobs )], 
              nclass = sqrt( length( fit[  !is.na( Yobs )] ) ), main = main )
    mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = main, 
                      main = main, gray.scale = gray.scale )
    #plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
  }
)

factor2num <- function(a){
  if(is.factor(a)){
    as.double(levels(a))[as.double(a)]
  } else{
    a
  }
}
factor2char <- function(a){
  levels(a)[as.numeric(a)]
}

round=base:::round
