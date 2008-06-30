# ==============================================================================
# S3 print function for mi object
# ==============================================================================
print.mi <- function ( x, ... ) {
  if ( !is.mi( x ) ) { 
    stop ( message = "Input data must have class 'mi'." ); 
  } else {
    n <- nrow(x$data)
    cat ( "\nMultiply imputed data set" );
    cat ( "\n\nCall:\n " );
    print( x$call );
    cat ( "\nNumber of multiple imputations: ", x$m,"\n");
    tab <- mi.info.table( x$mi.info )[,c("names","type","number.mis")]
    tab <- data.frame(tab, proportion=tab[,"number.mis"]/n )
    cat ( "\nNumber and proportion of missing data per column:\n" );
    print ( tab );
    cat ( "\nTotal Cases:", n );
    r    <- 1 * is.na ( x$data );
    cat ( "\nMissing at least one item:", sum ( colSums(r)!= 0 ) );
    cat ( "\nComplete cases:", sum ( rowSums(r) == 0 ), "\n" );
  }
  invisible( tab );
}
