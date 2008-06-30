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

setMethod("mi.resid", signature(object = "mi.logcontinuous"),     
  function ( object, Y ) {
    return( log(Y) - log(mi.expected ( object )) )
  }
)

setMethod("mi.plot", signature(object = "mi.logcontinuous"), 
function ( object, Yobs, main=deparse( substitute( Yobs ) ), gray.scale = FALSE, ... ) {
  par(mfrow=c(1,4))
  fit     <- mi.expected( object )
  res     <- mi.resid( object, Yobs )
  sigma   <- mi.sigma( object )
  vrb.obs <- Yobs
  vrb.imp <- mi.imputed( object, Yobs ) 
  loga  <- vrb.obs
  loga[!is.na(vrb.obs)] <-log(vrb.obs[!is.na(vrb.obs)])
  mi.hist( loga, object, type = vrb.typ, main = main, xlab=paste("log(",main,")"), gray.scale = gray.scale )
  mtext( "log", 1, cex = 0.7, adj = 1 )
  residual.plot( log(fit), res, sigma, main = main, xlab= "log(Predicted)",  ylab = paste( "log(Residual)" ), gray.scale = gray.scale )
  mtext( "log", 1, cex = 0.7, adj = 1 )
  mtext( "log", 2, cex = 0.7, adj = 1 )
  mi.scatterplot( loga, log(vrb.imp), fit, xlab= "predicted",  ylab = paste( "log(", main, ")" ) , main = main, gray.scale = gray.scale )
  mtext( "log", 2, cex = 0.7, adj = 1 )
  plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )
} 
)

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
