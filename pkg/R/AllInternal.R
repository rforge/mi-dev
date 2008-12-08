
vrb.typ <- NULL


factor2num <- function( a ) {
  if(is.factor( a ) ) {
    as.double( levels( a ) )[ as.double( a ) ]
  } else {
    a
  }
}


factor2char <- function( a ) {
  levels( a )[ as.numeric( a ) ]
}



# ========================================================================
# Extracts the imputation order vector(integer)
# ========================================================================

imp.order <- function(info){
  foo <- function(x) x$imp.order
  return(sapply(info, FUN=foo))
}

# ========================================================================
# Extracts the include or not vector (logical)
# ========================================================================

include <- function(info){
  foo <- function(x) x$include
  return(sapply(info, FUN=foo))
}

# ========================================================================
# Extracts the number of missing vector(integer)
# ========================================================================

nmis <- function(info){
  foo <- function(x) x$nmis
  return(sapply(info, FUN=foo))
}

# ========================================================================
# Extracts the all missing or not (logical) as vector
# ========================================================================

all.missing <-function(info){
  foo <- function(x) x$all.missing
  return(sapply(info, FUN=foo))
}
