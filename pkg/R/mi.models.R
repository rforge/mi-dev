# ==============================================================================
# The Imputation Model                     
# ==============================================================================
mi.models <- function ( data, filename = NULL, type.list = NULL, model.print = TRUE ,model.function = imputation.models, factor.type = "unordered-categorical" ) {
  if ( !is.data.frame ( data ) ) { stop ( message = "Data must be a data frame." ) }
  else if ( !is.null( type.list ) && ( length ( type.list ) != ncol ( data ) ) ) { stop ( message = "type.list must be specified for all variables." ) }
  else {
    col.mis    <- !complete.cases ( t ( data ) ) 
    ncol.mis   <- sum ( col.mis )
    model.list <- vector ( "list", ncol.mis )
    ncol.data  <- ncol( data )
    mis.index  <- seq ( ncol.data ) [ col.mis ]     
    col.names  <- names ( data ) # the names for the predictors 
    if ( !is.null ( type.list ) ) {
      type.list <- match.type( type.list )
      col.names [type.list == factor.type]  <- paste ( "factor (", col.names[type.list == factor.type], ")")  
    }
    type <- rep(NULL, ncol.mis )
    model.text <- paste( "## List of the models for imputing missing data", "\nstructure ( list ( \n ", sep="" )
    for ( j in 1:ncol.mis ) {
      i <- mis.index[j]
      type[j] <- ifelse ( is.null ( type.list ), datatype ( as.vector ( as.matrix ( data[,i] ) ) ), type.list[i] )
      names.preds   <- col.names [-i]  # the names for the predictors     
      model.default <- ""
      model.temp    <- model.function ( type[j] )
      model.default <- gsub ( "ImpVar", names( data )[i], model.temp )
      model.default <- gsub ( "ImpPar", paste ( names.preds, collapse = ", ",  sep="" ), model.default )
      model.list[[j]] <- model.default
      if ( !is.null ( filename ) ) {
        model.text <- paste( model.text, names(data)[i], " = \"", model.default, "\"", sep = "" )
      }
      if ( j == ncol.mis ) { model.text <- paste( model.text, " ),\n", sep="" ) }
      else { model.text <- paste( model.text, ",\n ", sep="" ) }
    }
    names ( model.list ) <- names ( data [ !complete.cases ( t ( data ) ) ] )
    if ( !is.null ( filename ) ) { 
      model.text <- paste ( model.text, "type = c( \n ", paste (paste(names(data[col.mis]),"='",type,"'",sep=""), collapse = " ,\n ", sep="" ), " ),\n", sep="" ) 
      model.text <- paste ( model.text, ".Names = c( ", paste ("\"",names(data[col.mis]), "\"", collapse = " , ", sep=""),sep="", " ) )")
      cat( model.text, file = filename, append = FALSE ) 
      cat( paste ( "Created File: ", getwd(), "/", filename, "\n", sep="" ) )
    }
    #if ( model.print == TRUE )  #print( model.list )
    names(type)<-col.names[mis.index]
    attr( model.list, "type" ) <- unlist(type)
    return( model.list )
  }
}

typelist<-function( data, len=3, text=FALSE ){
  return(
      if(!text){
        substr(sapply(data,datatype),1,len)
      }
      else{
        paste("c(", paste("'",substr(sapply(data,datatype),1,len),"'",collapse=", ",sep=""),")")
      }
  )
}

## To select the variables with the missing data
include.Y.obs <- function( data ) {
  Y.obs <- data.frame ( data [ complete.cases ( t ( data ) ) ] )
  names( Y.obs ) <- names( data ) [ complete.cases ( t ( data ) ) ]
  return ( obs.data = Y.obs )
}

## To impute the initial values in a matrix
include.Y.mis <- function ( data ) {
  Y.mis <- data.frame ( data [ !complete.cases ( t ( data ) ) ] )
  names ( Y.mis ) <- names ( data ) [ !complete.cases ( t ( data ) ) ]
  return ( mis.data = Y.mis )
}    
                         

## To dichotomize a variable
dicot <- function ( y ) {
  y <- ifelse( y == 0, 0, 1 )
}
