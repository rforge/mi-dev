# ==============================================================================
# missing pattern plot
# ==============================================================================
missing.pattern.plot <- function ( data, y.order = FALSE, x.order = FALSE, 
                                    xlab = "Index", ylab = "Variable", 
                                    main = NULL, gray.scale = FALSE,
                                    obs.col = "blue", mis.col = "red", ... ) {
  if ( is.null( main ) ) {
    main <- deparse( substitute( data ) )
  }
  index <- seq( nrow( data ) )
  if( y.order ) { 
    data <- data[ ,order( colSums( is.na( data ) ), decreasing = TRUE ) ] 
    ylab = "Variable in order of number of missing items" 
  }
  if( x.order ) { 
    data <- data[order( rowSums( is.na( data ) ), decreasing = FALSE ), ] 
    index<- row.names( data )
    xlab = "Index in order of number of missing items" 
  }
  col <- if( gray.scale ){ 
           gray( c( 0, 1 ) ) 
         } else { 
           c( obs.col,mis.col ) 
         }
  par( mar = c( 4.5, 11, 3, 1 ) )
  image( x = 1:nrow( data ), y = 1:ncol( data ), z = as.matrix( is.na( data ) ), 
          ylab = "", xlab = xlab, main = main, col = col ,yaxt = "n", ... )
  box( "plot" )
  axis( side = 2, at = 1:ncol( data ), labels = names( data ), las = 1, 
         tick = FALSE, yaxs = "r", tcl = 0.3, xaxs ="i", yaxs = "i" )
  mtext( ylab, side = 2 , line = 10)
  axis( side = 1, at = 1:nrow( data ), labels = index, tick = TRUE, 
          xaxs = "i", las = 1 )   
}

mp.plot <- function( data, ... ) {
  return( missing.pattern.plot( data = data, ... ) )
}
