# ==============================================================================
# The function to select the model based on the variable type
# ==============================================================================
match.type <- function ( type.list , type = mi.types() ) {  
  return( as.vector( sapply( type.list, match.arg, type ) ) )
}

mi.types <- function ( ) {
  type = c( "fixed"
            ,"continuous"
            ,"squareroot-continuous"
            ,"logscale-continuous"
            ,"ordered-categorical"
            ,"unordered-categorical"
            ,"dichotomous"
            ,"predictive-mean-match"
            ,"mixed" )
  return( type )
}

mi.types.add <- function ( type.name ){
  len <- length(body(mi.types)[[2]][[3]])+1
  body(mi.types)[[2]][[3]][[len]] <<- type.name
}

## To select the model 
imputation.models <- function ( type ) {
    type <- match.arg ( type, mi.types( ) )
    imputation.method <- list (
        "continuous"            = "mi.continuous( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
#        ,"logscale-continuous"  = "mi.logcontinuous( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
        ,"squareroot-continuous"= "mi.sqrtcontinuous( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
        ,"ordered-categorical"  = "mi.polr( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
        ,"unordered-categorical"= "mi.categorical( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
        ,"dichotomous"          = "mi.dichotomous( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
        ,"mixed"                = "mi.mixed( Y = ImpVar, X = cbind ( ImpPar ), check = FALSE )"
        ,"predictive-mean-match"= "mi.pmm( Y=ImpVar, X = cbind ( ImpPar ))"
        ,"fixed"                = "mi.fixed( Y = ImpVar, check = FALSE )"   
        ,"arithmetic-mean"      = "mi.mean( Y = ImpVar, check = FALSE )"  
        ,"median"               = "mi.median( Y = ImpVar, check = FALSE )"
    )
    return( imputation.method [ type ] ) # Will be NULL if the method is undefined
}

## To select the model 
type.models <- function ( type ) {
    type <- match.arg ( type, mi.types( ) )
    imputation.method <- list (
        "continuous"            = "mi.continuous"
        ,"logscale-continuous"  = "mi.logcontinuous"
        ,"squareroot-continuous"= "mi.sqrtcontinuous"
        #,"squareroot-continuous"="mi.continuous( imp.formula options)
        ,"ordered-categorical"  = "mi.polr"
        ,"unordered-categorical"= "mi.categorical"
        ,"dichotomous"          = "mi.dichotomous"
        #,"mixed"                ="mi.continuous"
        ,"mixed"                = "mi.mixed"
        ,"predictive-mean-match"= "mi.pmm"
        ,"fixed"                = "mi.fixed" 
    )
    return( imputation.method [ type ] ) # Will be NULL if the method is undefined
}
