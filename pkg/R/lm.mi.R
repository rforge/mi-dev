#==============================================================================
# Linear Regression for multiply imputed dataset
#==============================================================================
lm.mi <- function ( formula,  mi.object, ... ) 
{
    call   <- match.call( );
    m      <- m.mi( mi.object );
    result <- vector( "list", m );
    names( result ) <- as.character( paste( "Imputation", seq( m ), sep = ""));
    for ( i in 1:m ) {
        mi.data <- mi.matrix( mi.object, i );
        result[[i]] <- lm( formula, data = data.frame( mi.data ), ... );
    }
    coef   <- vector( "list", m );
    se     <- vector( "list", m );
    for( j in 1:m ) {
      coef[[j]]<-lapply( result, summary )[[j]]$coef[,1];
      se[[j]]  <-lapply( result, summary )[[j]]$coef[,2];
    }
    W         <- colSums( do.call( rbind, se ) ^ 2 ) / m;
    Bhat      <- colSums( do.call( rbind, coef ) ) / m;
    Bhat_rep  <- t(matrix( rep( Bhat, m ), length(Bhat), m ));
    B         <- colSums( do.call( rbind, coef ) - Bhat_rep ) ^ 2 / ( m - 1 );
    pooled <- list( coefficient = NULL, se = NULL );
    pooled$coefficient    <- Bhat;
    pooled$se <- sqrt( W + ( 1 + 1 / m ) * B );  
    mi.lm.object <- list( call = call, lm.mi.pooled = pooled, 
                          lm.mi.fit = result );
    class( mi.lm.object ) <- c( "mi.lm", "list" );
    return( mi.lm.object );
}

print.mi.lm<-function( object ){
  cat( "----------------------------------------\n" )
  cat( "Pooled Estimate\n" )
  cat( "----------------------------------------\n" )
  print( object$call )
  tab<-cbind( object$lm.mi.pooled[[1]], object$lm.mi.pooled[[2]] )
  dimnames( tab )[[2]] <- c( "coef.est", "coef.se" )
  print( tab )
  cat( "---\n" )
  cat( "----------------------------------------\n" )
  cat( "Separate Estimate for each Imputation\n" )
  cat( "----------------------------------------\n" )
  for( i in 1:length( object$lm.mi.fit ) ) {
    display( object$lm.mi.fit[[i]] )
    cat( "----------------------------------------\n" )
  }
}

#glm.mi.norm <- function (formula,mi.object,  family = gaussian, ...) 
#{
#    call <- match.call()
#    m      <- m.mi(mi.object)
#    result <- vector("list",m)
#    for (i in 1:m) {
#        mi.data <- mi.matrix.mi.norm(mi.object, i)
#        result[[i]] <- glm(formula, family=family, data = data.frame(mi.data), ...)
#    }
#    pooled <- list(coefficient=NULL, se=NULL )
#    coef <-list()
#    se <-list()
#    for(j in 1:m) {
#      coef[[j]]<-lapply(result,summary)[[j]]$coef[,1]
#      se[[j]]<-lapply(result,summary)[[j]]$coef[,2]
#    }
#    W<-colSums(do.call(rbind,se)^2)/m
#    Bhat <- colSums(do.call(rbind,coef))/m
#    Bhat_rep <- t(matrix(rep(Bhat,m),length(Bhat),m))
#    B <-colSums(do.call(rbind,coef) - Bhat_rep )^2/(m-1)
#    pooled$se <- sqrt(W+(1+1/m)*B)
#    pooled$coefficient <- Bhat
#    mi.glm.object <- list(call = call, lm.mi.pooled = pooled, glm.mi.fit = result )
#    class( mi.glm.object ) <- c("mi.glm","list")
#    return( mi.glm.object )
#}