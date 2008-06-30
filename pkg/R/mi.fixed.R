# ==============================================================================
# imputation function for fixed variable
# ==============================================================================
mi.fixed <- function( formula, data = NULL, ... ) {
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
  namesD <- if( is.null( data ) ) { 
              NULL 
            } 
            else { 
              deparse( substitute( data ) )
            }
  nameY <- deparse( substitute( Y ) );
  mis   <- is.na( Y );
  n.mis <- sum ( mis );
  y.level <-  if ( is.numeric( Y ) ) {
                sort( unique ( Y ) );
              } else {
                levels( factor( Y ) );
              }
  # main program
  fixd.imp    <- y.level;
  determ.pred <- rep( y.level, length( Y ) );
  names( determ.pred ) <- 1:length( determ.pred );
  random.pred <- determ.pred[is.na(Y)];
  # calculate residual
  #residual.val<- Y - determ.pred
  # return the result
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), 
                  expected = NULL, random = NULL );
  result$model$call    <- ""
  result$expected <- determ.pred
  result$random   <- random.pred
  #result$residual <- residual.val
  class ( result ) <- c( "mi.fixed", "mi.method","list" )
  return( result )
  on.exit( rm( fixd.imp) )
}

mi.copy <- function( Y, X, ... ) {
  nameY <- deparse( substitute( Y ) )
  nameX <- deparse( substitute( X ) )
  mis   <- is.na( Y )
  n.mis <- sum ( mis )
  # main program
  fixd.imp    <- nameX
  determ.pred <- unlist(X)
  names( determ.pred ) <- 1:length( determ.pred )
  random.pred <- determ.pred[mis]
  # return the result
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), expected = NULL, random = NULL )
  result$expected <- determ.pred
  result$random   <- random.pred
  #result$residual <- residual.val
  class ( result ) <- c( "mi.copy", "mi.method","list" )
  return( result )
  on.exit( rm( fixd.imp) )
}

setMethod("mi.plot", signature(object = "mi.copy"), 
 function ( object, Yobs, main=deparse( substitute( Yobs ) ), gray.scale = FALSE ) {
  #par(mfrow=c(1,4))
#  fit     <- mi.expected( object )
#  res     <- mi.resid( object, Yobs )
#  sigma   <- mi.sigma(object)
#  vrb.obs <- Yobs
#  vrb.imp <- mi.imputed( object, Yobs )
#  fit1 <- fit[[1]]
#  fit2 <- fit[[2]]
#  res1 <- res[[1]]
#  res2 <- res[[2]]    
#  mi.hist ( vrb.obs, object, xlab = main, main = main, gray.scale = gray.scale )
#  binnedplot( fit1[ !is.na( Yobs ) ], res1[ !is.na( Yobs ) ], nclass=sqrt(length(fit1[ !is.na( Yobs ) ])), main = main)
#  residual.plot ( fit2, res2, sigma, main = main, gray.scale = gray.scale )
#  #mtext( "sqrt", 2, cex = 0.7, adj = 1 )
#  mi.scatterplot ( vrb.obs, vrb.imp, xlab = "predicted", ylab = main, main = main, gray.scale = gray.scale, display.zero=FALSE )
}
)
