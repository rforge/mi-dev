# ==============================================================================
# imputation function using predictive mean matching
# ==============================================================================
mi.pmm<-function(formula, data = NULL, start = NULL, n.iter = 100, ... )
{
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
  if(!is.null(start)){n.iter<-1}
  bglm.imp  <- bayesglm( formula , start = start, n.iter = n.iter )
  #sim.bglm.imp    <- sim( bglm.imp,1 )
  #yhat            <- X[mis,] %*% sim.bglm.imp$beta
  yhat <- predict( bglm.imp , newdata = data.frame( Y, X ) ) 
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), 
                    expected = NULL, random = NULL, residual = NULL )
  result$model$call <- bglm.imp$call
  result$model$coefficient <- bglm.imp$coefficients
  result$model$sigma <- sigma.hat( bglm.imp )
  result$expected <- yhat
  result$random   <- apply( as.array( yhat[mis] ), 1, 
                              mi.pmm.match, yhat=yhat[!mis], Y=Y[!mis] ) 
  result$residual <- bglm.imp$residuals
  class ( result )<- c( "mi.pmm", "mi.method","list" )
  return( result )

}

mi.pmm.match<-function(z, yhat=yhat, Y=Y)
{
    d <- abs( yhat - z )
    m <- Y[ d == min( d )]
    if ( length( m ) > 1 ) m <- sample( m, 1 )
    return( m )
}

setMethod("mi.hist", signature(object = "mi.pmm"),  
 function ( Yobs, object, main = paste("Histogram of ", deparse( substitute( Yobs ) )),  
             gray.scale = FALSE, xlab = deparse( substitute( Yobs ) ), ylab = "Frequency", 
             b = NULL, binwidth = NULL, col = c( "black", "blue", "red" ), 
             lty = c( 1, 1, 1 ), lwd = c( 1, 1, 1 ), mlt = 0.1, ... )
{
  Yimp <-mi.imputed(object,Yobs)
  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if( is.null( binwidth ) ) { binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )}
  if( is.null( b )) { b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )}
  if( gray.scale == TRUE ) { 
    col <- c( gray( 0.8 ), gray( 0.6 ), gray( 0 ) ) 
    lty <- c( 3, 1, 1 )
  }
  #b <- seq( 0, ceiling( max( Yimp ) ), 0.2 )
  h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
  h.mis <- hist( mis, plot = FALSE, breaks = b )
  h.imp <- hist( Yimp, plot = FALSE, breaks = b )
  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
  xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
  lab <- as.double( names( table( obs.nomis ) ) )
  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
  histlineplot ( h.mis, shift=-mlt*binwidth, col=col[3], lty = lty[3], lwd = lwd[3] )
  histlineplot ( h.obs, shift=mlt*binwidth, col=col[2], lty = lty[2], lwd = lwd[2] ) 
  histlineplot ( h.imp, col=col[1] , lty = lty[1], lwd = lwd[1] )  
  axis( 1, lab, tick = TRUE, col.axis = 'black' )
}
)

print.mi.pmm <- function ( object ) {
  cat("model:\n ")
  print(object$model$call)
  cat("\ncoefficients:\n ")
  print(object$model$coefficient)
  cat("\nimputed values:\n")
  print(object$random)
}
