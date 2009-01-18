#==============================================================================
# Generalized Linear Modeling for multiply imputed dataset
#==============================================================================
glm.mi <- function ( formula, mi.object = NULL, data.list = NULL, family = gaussian, ... ) 
{
    call   <- match.call( )
    if(is.null(data.list)){
      m      <- m( mi.object ) 
      result <- vector( "list", m )
      names( result ) <- as.character(paste( "Imputation", seq( m ), sep = "" ))
      for ( i in 1:m ) {
          mi.data     <- mi.completed( mi.object, i )
          result[[i]] <- glm( formula, family = family, 
                                data = data.frame( mi.data ), ... )
      }
    }
    else{
      m <- length(data.list)
      result <- vector( "list", m )
      names( result ) <- as.character(paste( "Imputation", seq( m ), sep = "" ))
      for ( i in 1:m ) {
          mi.data     <- mi.data.list(mi.object)
          result[[i]] <- glm( formula, family = family, 
                                data = data.frame( mi.data[[i]] ), ... )    
      }
    }
    coef   <- vector( "list", m )
    se     <- vector( "list", m )
    for( j in 1:m ) {
      coef[[j]]<- lapply( result, summary )[[ j ]]$coef[ ,1]
      se[[j]]  <- lapply( result, summary )[[ j ]]$coef[ ,2]
    }
    W         <- colSums( do.call( rbind, se )^2 ) / m
    Bhat      <- colSums( do.call( rbind, coef ) ) / m
    Bhat_rep  <- t( matrix( rep( Bhat, m ), length( Bhat ), m ) )
    B         <- colSums( do.call( rbind, coef ) - Bhat_rep ) ^ 2 / ( m - 1 )
    
    pooled <- list( coefficient = NULL, se = NULL )
    pooled$coefficient <- Bhat
    pooled$se <- sqrt( W + ( 1 + 1 / m ) * B )

    mi.glm.object <- list( call = call, glm.mi.pooled = pooled,
                                  glm.mi.fit = result )
    class( mi.glm.object ) <- c( "mi.glm", "list" )
    return( mi.glm.object )
}
