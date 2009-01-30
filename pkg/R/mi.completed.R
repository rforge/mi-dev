mi.completed.default <- function(object, m = 1){
  outcome<- match.arg(outcome)
  if( m(object) < m )  { stop( message = "Index of imputation is not within the range." ) }
  info <- info.mi(object)
  mis.name <- names(.nmis(info)[.nmis(info) > 0 & !.all.missing( info ) ] )
  mimatrix <- data.mi(object)
  for ( i in 1:length(mis.name) ){
    nm <- mis.name[i]
    mimatrix[ ,nm] <- imputed(imp(object,m)[[nm]], mimatrix[ ,nm] )
  }
}  
  



setMethod( "mi.completed", signature( object = "mi" ),
  function (object, m = 1, outcome = c("data.frame","matrix", "all")) {
    if(outcome=="data.frame"){
      mimatrix <- mi.completed.default(object, m)
      return( data.frame( mimatrix ) )
    } 
    else if(outcome=="matrix"){
      mimatrix <- mi.completed.default(object, m)
      return( as.matrix( mapply(.factor2num, mimatrix) ) )
    }
    else{
      data <- vector("list", n.chains)
      for(i in 1:m(object)){
        data[[i]] <- mi.completed(object, m = i, outcome = "data.frame") 
      }
      return(data)
    }
  }
)


setMethod( "mi.matrix", signature( object = "mi" ),
  function ( object, m = 1) {
    mi.completed( object, m = m, outcome = "matrix") 
  }
)


setMethod( "mi.data.frame", signature( object = "mi" ),
  function ( object, m = 1) {
    mi.completed( object, m = m, outcome = "data.frame") 
  }
)
