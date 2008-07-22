mi.logcontinuous <- function( formula, data = data.frame ( cbind ( Y, X ) ),
                              start = NULL, n.iter = 100, ... ) {
  call <- match.call()
  mf   <- match.call(expand.dots = FALSE)
  m    <- match(c("formula", "data"), names(mf), 0)
  mf   <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  Y  <- model.response(mf, "any")
  if (length(dim(Y)) == 1) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm)) 
      names(Y) <- nm
  }
  X <- mf[,-1,drop=FALSE]
  namesD <- if( is.null( data ) ) { NULL } else { deparse( substitute( data ) ) }
  mis    <- is.na( Y )
  n.mis  <- sum( mis )
  if(is.null(data)){ data<- mf }
  # main program
  if (sum(1*is.positive(X)) > 0) {
    namesX[is.positive(X)]<-paste( "log(",namesX[is.positive(X)],")" )
    X[,is.positive(X)]<-log( X[,is.positive(X)] )
  }
  if( !is.null( start ) ){ n.iter <- 1 } 
  #control<-if(!is.null(start)){glm.control(maxit=1)}else{glm.control(...)}
  #bglm.imp        <- bayesglm( formula = log ( Y ) ~ X, data = data, family = gaussian, n.iter = n.iter, start = start, control=control )
  bglm.imp        <- bayesglm( formula = log ( Y ) ~ X, data = data, 
                                family = gaussian, n.iter = n.iter, 
                                  start = start )
  determ.pred     <- predict( bglm.imp, newdata = data.frame( Y, X ), 
                                type = "response" )
  random.pred     <- rnorm( n.mis, determ.pred[mis], sigma.hat( bglm.imp ) )
  names( random.pred ) <- names( determ.pred[mis] )
  # calculate residual
  # return the result
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), expected = NULL, random = NULL )
#  result$model$call <- paste( "bayesglm(formula = log(", namesY, ") ~ ",
#                                          paste( namesX, collapse = " + " ),
#                                          ", family = gaussian, ",
#                                          if( !is.null( namesD ) ){ paste( "data = ", namesD, ", ", sep="" ) }, 
#                                          if( !is.null( start ) ) { paste( "start = ", paste( start, collapse = "," ), ", ") },
#                                          "n.iter = ", n.iter, ")", sep="" )
  result$model$call        <- bglm.imp$call
  result$model$call$formula<- as.formula( formula );
  result$model$call$start  <- round(as.double( start ), 2 );
  result$model$call$n.iter <- n.iter;
  result$model$coefficient <- bglm.imp$coefficients
  result$model$sigma <- sigma.hat( bglm.imp )
  result$model$dispersion  <- bglm.imp$dispersion
  result$expected <- exp( determ.pred )
  result$random   <- exp( random.pred )
  class ( result )<- c( "mi.logcontinuous", "mi.method", "list" )
  return( result )
  on.exit( rm( bglm.imp ) )
}


is.positive <- function ( data ) {
    len<-ncol(data) 
    result <- logical( len )
    names(result) <- names(data)
    for ( i in 1:len ) {
        if( is.numeric(data[!is.na(data[,i]),i])){
            result[i]<-(min(data[!is.na(data[,i]),i])>0)
        }
        else{ 
            result[i] <- NA 
        }
    }
    return(result)
}
