setMethod( "mi.completed", signature( object = "mi" ),
  function ( object, m = 1, outcome = c("data.frame","matrix") ) {
    outcome<- match.arg(outcome);
    if(outcome=="data.frame"){
      return(mi.data.frame(object,m));
    } else {
      return(mi.matrix(object,m));
    }
  }
)
setMethod( "mi.matrix", signature( object = "mi" ),
  function ( object, m = 1 ) {
    if( m(object) < m )  { stop( message = "Index of imputation is not within the range." ) }
    info <- info.mi(object)
    mis.name <- names( nmis(info)[ nmis( info ) > 0 & !all.missing( info ) ] );
    mimatrix <- data.mi(object);
    for ( i in 1:length(mis.name) ){
      nm <- mis.name[i]
      mimatrix[ ,nm] <- imputed( imp(object,m)[[nm]],mimatrix[ ,nm] );
    }  
    mimatrix<-mapply(factor2num,mimatrix);
    return( as.matrix( mimatrix ) );
  }
)
setMethod( "mi.data.frame", signature( object = "mi" ),
  function ( object, m = 1 ) {
  if( !inherits ( object, "mi" ) ) { stop( message = "Object must be 'mi' class." ) }
  if( m(object)< m )  { stop( message = "Index of imputation is not within the range." ) }
  info <- info.mi(object)
  mis.name <- names( nmis(info)[ nmis( info ) > 0 & !all.missing( info ) ] );  
  mimatrix <- data.mi(object);
  for ( i in 1:length(mis.name) ){
      nm <- mis.name[i]
      mimatrix[ ,nm] <- imputed( imp(object,m)[[nm]],mimatrix[ ,nm] );
  }  
  return( data.frame( mimatrix ) )
}
)
