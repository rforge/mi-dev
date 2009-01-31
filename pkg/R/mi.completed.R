mi.completed.default <- function(object, m = 1){
  if( m(object) < m )  { stop( message = "Index of imputation is not within the range." ) }
  info <- info.mi(object)
  mis.name <- names(.nmis(info)[.nmis(info) > 0 & !.all.missing( info ) ] )
  mimatrix <- data.mi(object)
  for ( i in 1:length(mis.name) ){
    nm <- mis.name[i]
    mimatrix[ ,nm] <- imputed(imp(object,m)[[nm]], mimatrix[ ,nm] )
  }
  return(as.data.frame(mimatrix))
}  
  



setMethod("mi.completed", signature( object = "mi" ),
  function (object) {
    n.chains <- m(object)
    data <- vector("list", n.chains)
    for(i in 1:m(object)){
      data[[i]] <- mi.completed.default(object, m = i) 
    }
    return(data)
  }
)




setMethod( "mi.data.frame", signature( object = "mi" ),
  function ( object, m = 1) {
    mi.completed.default( object, m = m) 
  }
)
