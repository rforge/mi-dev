# ========================================================================
# preprocessing the data
# ========================================================================

preprocess.data <- function(data){
  n.col <- ncol(data)
  n.row <- nrow(data)
  var.name <- names(data)
  type <- apply(data, 2, typecast)
  idx1 <- match(type, "mixed", nomatch=0)
  n.new.var <- sum(idx1)
  idx2 <- seq(1, n.new.var)
  TMP.lab <- NULL
  TMP <- NULL
  for(i in 1:n.col){
    if(idx1[i]){
      tmp1 <- ifelse(data[,i] > 0, 1, ifelse(data[,i]==0, 0, NA))
      tmp2 <- log(ifelse(data[,i] > 0, data[,i], NA))
      tmp1.lab <- paste(var.name[i], "ind", sep=".")
      tmp2.lab <- paste(var.name[i], "log", sep=".")
      TMP.lab <- c(TMP.lab, tmp1.lab, tmp2.lab)
      TMP <- cbind(TMP, tmp1, tmp2)
    }
  }
  colnames(TMP) <- TMP.lab
  data <- data[,-idx2]
  data <- cbind.data.frame(TMP, data)
  return(as.data.frame(data))
  on.exit(rm(data))
  on.exit(rm(TMP))
}


postprocess.data <- function(trans.data, org.data){
  var.name <- names(org.data)
  idx1 <- pmatch(names(trans.data), names(org.data), nomatch=0)
  idx1 <- ifelse(idx1==0, 1, 0)
  idx1 <- seq(1, sum(idx1), 1)
  data <- trans.data[,idx1]
  idx2 <- pmatch(names(org.data), names(trans.data), nomatch=0)
  idx2 <- pmatch(0, idx2)
  TMP.lab <- var.name[idx2]
  n.col <- ncol(data)
  TMP <- NULL
  for(i in seq(1,(n.col-1),2)){
    tmp <- data[,i]*exp(data[,(i+1)])
    TMP <- cbind(TMP, tmp)
  }
  names(TMP) <- TMP.lab
  trans.data <- trans.data[,-idx1]
  trans.data <- cbind.data.frame(TMP, trans.data)
  return(trans.data)
  on.exit(rm(data))
  on.exit(rm(TMP))
}




vrb.typ <- NULL # to pass R CMD check
data.tmp <<- NULL # to pass R CMD check


# ========================================================================
# Random draw from the obs. data
# ========================================================================

.randdraw <- function(data, n = 1){
  foo <- function(x) sample(na.exclude(x), size = n, replace = FALSE)
  added.rows <- apply(data, 2, FUN = foo)
  return(added.rows)
}


# ========================================================================
# Convert factor to numeric value
# ========================================================================


.factor2num <- function( a ) {
  if(is.factor( a ) ) {
    as.double( levels( a ) )[ as.double( a ) ]
  } else {
    a
  }
}

# ========================================================================
# Convert character value
# ========================================================================

.factor2char <- function( a ) {
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
