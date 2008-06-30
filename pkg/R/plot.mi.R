# ==============================================================================
# S3 plot function for mi object
# ==============================================================================
plot.mi <- function ( x, m = 1, vrb = NULL, vrb.name = "Variable Score",
                        gray.scale = FALSE, mfrow=c( 1, 4 ), ... ) {
  if ( !is.mi( x ) ) { 
    stop( message = "Object must be 'mi' class." ) 
  } else if ( x$m < m )  { 
    stop( message = paste( "Index of imputation 'm' must be within the range of 1 to", x$m ) ) 
  } else{
    mids <- x$imp[[ m ]];
    Y    <- as.data.frame( x$data[ , names( mids ) ] );
    names( Y ) <- names( mids );
    par( mfrow = mfrow );
    for( i in 1:dim( Y )[2] ) {
      par( ask = TRUE );
      if( !is.null( mids[[i]] ) ) {
        mi.plot( mids[[i]], Y[ ,names( mids )[i]], main = names( Y )[ i ] );
      }
    }
  }
}
