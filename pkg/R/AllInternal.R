
prior.control <- function(augment.data = FALSE, pct.aug=10, K = 0){
  return(list(augment.data = augment.data, pct.aug = pct.aug, K = K))
}


# ========================================================================
# create missingness
# ========================================================================

.create.missing <- function(data, pct.mis=10){
  n <- nrow(data)
  J <- ncol(data)
  if(length(pct.mis)==1){
    n.mis <- rep((n*(pct.mis/100)), J)
  }
  else{
    if(length(pct.mis) < J) stop("The length of missing does not equal to the column of the data")
    n.mis <- n*(pct.mis/100)
  }
  for(i in 1:ncol(data)){
    if(n.mis[i]==0){
      data[,i] <- data[,i]
    }
    else{
      data[sample(1:n, n.mis[i], replace=FALSE),i] <- NA
    }
  }
  return(data)
}
  



.check.log.var <- function(x){
  check1 <- min(x, na.rm=TRUE) < 0
  if(check1) stop("log cannot take on negative values")
  check2 <- min(x, na.rm=TRUE) == 0 
  if(check2){
    k <- round((min(x[x>0], na.rm=TRUE) + 0)/2,2)
    return(k)
  }
  else return(0)
}



# ========================================================================
# preprocessing the data
# ========================================================================
.preprocess.data <- function(data){
  n.col <- ncol(data)
  n.row <- nrow(data)
  var.name <- names(data)
  type <- apply(data, 2, typecast)
  idx1 <- match(type, "mixed", nomatch=0)
  n.new.var <- sum(idx1)
  idx2 <- grep("mixed", type)
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


.postprocess.data <- function(trans.data){
  n.chains <- length(trans.data)
 # if(!is.null(org.data)){
#    var.name <- names(org.data)
#    idx1 <- pmatch(names(trans.data[[1]]), names(org.data), nomatch=NA)
#    idx1 <- na.exclude(idx1)
#    TMP.lab <- var.name[-idx1]
#    idx1 <- pmatch(names(trans.data[[1]]), names(org.data), nomatch=0)
#    idx1 <- grep(0, idx1)
#    n.col <- max(idx1)
#    for (s in 1:n.chains){
#      data <- trans.data[[s]][,idx1]
#      trans.data[[s]] <- trans.data[[s]][,-idx1]
#      TMP <- NULL
#      for(i in seq(1,(n.col-1),2)){
#        tmp <- data[,i]*exp(data[,(i+1)])
#        TMP <- cbind(TMP, tmp)
#      }
#    colnames(TMP) <- TMP.lab
#    trans.data[[s]] <- cbind.data.frame(TMP, trans.data[[s]])
#    }
#  }
#  else{
#    org.data <- data.mi(mi.object)
  var.name <- names(trans.data[[1]])
  idx1 <- grep("ind", names(trans.data[[1]]))
  idx2 <- grep("log", names(trans.data[[1]]))
  idx3 <- c(idx1, idx2)
  var.name1 <- var.name[idx1]
  var.name1 <- gsub(".ind", "", var.name1)
  var.name <- c(var.name1, var.name[-idx3])
  for (s in 1:n.chains){
    data <- trans.data[[s]][,idx1]*exp(trans.data[[s]][,idx2])
    trans.data[[s]] <- trans.data[[s]][,-idx3]
    trans.data[[s]] <- cbind.data.frame(data, trans.data[[s]])
    names(trans.data[[s]]) <- var.name
  }  
#  }
  return(trans.data)
}




vrb.typ <- NULL # to pass R CMD check
data.tmp <<- NULL # to pass R CMD check


# ========================================================================
# Random draw from the obs. data
# ========================================================================

.randdraw <- function(data, n = 1){
  foo <- function(x) sample(na.exclude(x), size = n, replace = TRUE)
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
# Extracts the type (character) as vector
# ========================================================================

type <-function(info){
  foo <- function(x){
    x$type
  }
  type <- sapply(info, FUN = foo)
  return(type)
}

# ========================================================================
# Extracts the level (vector) as list
# ========================================================================

level <- function(info){
  foo <- function(x){
    x$level
  }
  level <- sapply(info, FUN = foo)
  return(level)
}

# ========================================================================
# Extract imputation formula (character) as list
# ========================================================================

imp.formula <-function(info){
  foo <- function(x){
    x$imp.formula
  }
  form <- sapply(info, FUN = foo)
  return(form)
}

# ========================================================================
# Extracts the imputation order vector(integer)
# ========================================================================

imp.order <- function(info){
  foo <- function(x){
    x$imp.order
  }
  imp.order <- sapply(info, FUN = foo)
  return(imp.order)
}

# ========================================================================
# Extracts the include or not vector (logical)
# ========================================================================

include <- function(info){
  foo <- function(x){
    x$include
  }
  include <- sapply(info, FUN = foo)
  return(include)
}

# ========================================================================
# Extracts the number of missing vector(integer)
# ========================================================================

nmis <- function(info){
  foo <- function(x){
    x$nmis
  }
  nmis <- sapply(info, FUN=foo)
  return(nmis)
}

# ========================================================================
# Extracts the all missing or not (logical) as vector
# ========================================================================

all.missing <-function(info){
  foo <- function(x){
    x$all.missing
  }
  all.missing <- sapply(info, FUN=foo)
  return(all.missing)
}
