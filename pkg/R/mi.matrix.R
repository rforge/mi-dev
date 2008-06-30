# ==============================================================================
#  create imputation
# ==============================================================================

mi.matrix <- function ( mi.object, m = 1 ) {
  if( !inherits ( mi.object, "mi" ) ) { stop( message = "Object must be 'mi' class." ) }
  if( mi.object$m < m )  { stop( message = "Index of imputation is not within the range." ) }
  mis.name <- names( nmis(mi.object$mi.info)[ nmis( mi.object$mi.info ) > 0 & !all.missing( mi.object$mi.info ) ] );
  mimatrix <- mi.object$data;
  for ( i in 1:length(mis.name) ){
    mimatrix[ ,mis.name[i]] <- mi.imputed( mi.object$imp[[m]][[mis.name[i]]],mimatrix[ ,mis.name[i]] );
  }  
  mimatrix<-mapply(factor2num,mimatrix);
  return( as.matrix( mimatrix ) );
}

mi.data.frame <- function ( mi.object, m = 1 ) {
  if( !inherits ( mi.object, "mi" ) ) { stop( message = "Object must be 'mi' class." ) }
  if( mi.object$m < m )  { stop( message = "Index of imputation is not within the range." ) }
  mis.name <- names( nmis(mi.object$mi.info)[ nmis( mi.object$mi.info ) > 0 & !all.missing( mi.object$mi.info ) ] );
  mimatrix <- mi.object$data;
  for ( i in 1:length(mis.name) ){
    mimatrix[ ,mis.name[i]] <- mi.imputed( mi.object$imp[[m]][[mis.name[i]]],mimatrix[ ,mis.name[i]] );
  }  
  return( data.frame( mimatrix ) )
}
