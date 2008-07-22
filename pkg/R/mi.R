#==============================================================================
# mi main function
#==============================================================================

mi <- function ( object, info, type = NULL, n.imp = 3, n.iter = 30, 
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                   preprocess = FALSE, continue.on.convergence = FALSE,
                    seed = NA, check.coef.convergence = FALSE ) {
  call <- match.call( );                         # call
  if( !is.na ( seed ) ) { set.seed( seed ); }    # set random seed
  ProcStart     <- proc.time();                  # starting time
  # variable initialization
  time.out.flg  <- FALSE;
  converged.flg <- FALSE;
  max.iter.flg  <- FALSE;
  Time.Elapsed  <- 0;
  con.check     <- NULL;

  if( class( object ) %in% c( "matrix", "data.frame" ) ) { 
  # For data frame and matrix  
    object     <- data.frame( object );
    nameD      <- deparse( substitute( object ) );
    org.data   <- object;
    data       <- object;
    col.mis    <- !complete.cases( t( data ) ); 
    ncol.mis   <- sum( col.mis );
    if( missing( info ) ) {     
      info <- mi.info( data );    # create mi.info
    }      
    AveVar  <- array( NA, c( n.iter, n.imp, dim( data[,info$include] )[2] * 2 ) );
    s_start <- 1;
    s_end   <- n.iter;
       
  } else if ( class( object ) == "mi" ) {
  # for mi object

    org.data  <- data.mi(object);
    data      <- data.mi(object);
    col.mis   <- !complete.cases( t( data ) );
    ncol.mis  <- sum( col.mis );
    n.imp     <- m(object);
    info      <- info.mi(object);
    prev.iter <- dim(bugs.mi(object)$sims.array)[1]
    AveVar    <- array( NA, c( prev.iter + n.iter,
                                 n.imp, 
                                  sum( include( info ) ) * 2 ) );
    AveVar[ 1:prev.iter , , ]<- bugs.mi(object)$sims.array;
    s_start <- prev.iter + 1;
    s_end   <- prev.iter + n.iter;
  } else {
  # unexpected object
    stop( gettextf( "object class '%s' is not acceptable", class( object ) ) );
  }
  
  mis.index <-  apply(data, 2, is.na)
  # Automatic Preprocess

  if( preprocess ) {
    data <- mi.info.recode( data, info );
  }
 
  data<-data[,include( info ) ];
  dimnames( AveVar ) <- list( NULL, NULL, 
                              c( paste( "mean(", colnames( data ),")",sep="" ), 
                                 paste( "sd(", colnames( data ), ")", sep="" ) ) );
  VarName.tm <- names( info )[include( info ) & nmis( info )>0 ];
  VarName    <- VarName.tm[order(imp.order( info )[include( info ) & nmis( info )>0 ])];
  length.list<- sum( include( info ) & nmis( info ) >0 );
  # list initialization
  mi.data       <- vector( "list", n.imp );
  start.val     <- vector( "list", n.imp );
  mi.object     <- vector( "list", n.imp );
  for (j in 1:n.imp){ 
    mi.data[[j]]  <-  if( class( object ) %in% "mi" ){ 
                        data.frame( mi.matrix( object, m=j )[,include( info )] ); 
                      } else{ 
                        random.imp( data, method = rand.imp.method );
                      }
    start.val[[j]]<- vector( "list", length.list );
    mi.object[[j]]<- vector( "list", ncol.mis );
    names(mi.object[[j]]) <- names( info )[ nmis( info )>0 ];
  }
################################################################
#  #Retro Grade residual codes
#  exp.val <- vector("list",n.imp)
#  prd.val <- vector("list",n.imp)
#  for (jjj in 1:n.imp){
#  exp.val[[jjj]]<- vector("list", n.iter)
#  prd.val[[jjj]]<- vector("list", n.iter)
#    for(kkk in 1:n.iter){
#      exp.val[[jjj]][[kkk]] <- vector("list",length(VarName))
#      names(exp.val[[jjj]][[kkk]])<-VarName.tm 
#      prd.val[[jjj]][[kkk]] <- vector("list",length(VarName))
#      names(prd.val[[jjj]][[kkk]])<-VarName.tm 
#    }
#  }
################################################################
  coef.val <- vector("list",ncol.mis)
  names(coef.val) <- names( info )[ nmis( info )>0 ];
  for (jjj in 1:ncol.mis){
    coef.val[[jjj]]<- vector("list", n.imp)
  }
  names(mi.object)<- paste( "Imputation", 1:n.imp, sep="" );
  cat( "Beginning Multiple Imputation (", date(), "):\n" );
  # iteration loop
  for ( s in s_start:s_end ) {
    cat( "Iteration", s,"\n" );
    # imputation loop
    for ( i in 1:n.imp ){
      cat( " Imputation", i,  ": " );
      # variable loop
      for( jj in 1:length(VarName) ) {
        CurrentVar <- VarName[jj];
        cat( CurrentVar, " " );
        CurVarFlg <- ( names ( data ) == CurrentVar )
        dat <- data.frame( data[ ,CurVarFlg, drop=FALSE ], 
                            mi.data[[i]][ ,!CurVarFlg ] );
        names( dat ) <- c( CurrentVar, names( data[,!CurVarFlg, drop=FALSE] ) );
        model.type   <- as.character( type.models( info[[CurrentVar]]$type ) );
        
        # Error Handling
        .Internal(seterrmessage(""));
        errormessage <- paste("\nError while imputing variable:", CurrentVar, ", model:",model.type,"\n")
        on.exit ( cat(errormessage,geterrmessage()));
        on.exit ( options( show.error.messages = TRUE ),add = TRUE);
        options( show.error.messages = FALSE );
        # Error Handling
        mi.object[[i]][[CurrentVar]] <- with( dat, 
                                          do.call( model.type,
                                                    args = c( list( formula = info[[CurrentVar]]$imp.formula, 
                                                    data = dat,
                                          start=if(!is.null( start.val[[i]][[jj]] ) ){
                                                  start.val[[i]][[jj]];
                                                }
                                                else{
                                                  NULL;
                                                }
                                        ),
                                          info[[CurrentVar]]$params
                              ) ) );
        # Error Handling
        on.exit ();
        options( show.error.messages = TRUE );
        # Error Handling
################################################################
#        #Retro Grade residual codes
#        prd.val[[i]][[s]][[CurrentVar]]<- mi.data[[i]][[CurrentVar]]
#        exp.val[[i]][[s]][[CurrentVar]]<- mi.object[[i]][[CurrentVar]]$expected
        mi.data[[i]][[CurrentVar]][is.na( data[[CurrentVar]] )] <- mi.object[[i]][[CurrentVar]]$random;
        coef.val[[CurrentVar]][[i]]<-rbind(coef.val[[CurrentVar]][[i]],unlist(mi.coef(mi.object[[i]][[CurrentVar]])));
        start.val[[i]][[jj]] <- mi.start( mi.object[[i]][[CurrentVar]] );
      } ## variable loop 
      cat("\n" );
      #AveVar[s,i,] <- c( mean( mi.data[[i]] ),sd( mi.data[[i]] ) )
      AveVar[s,i,] <-c(sapply(mi.data[[i]],function(v){mean(unclass(v),na.rm=T)}),
                        sapply(mi.data[[i]],function(v){sd(unclass(v),na.rm=T)}));
                        
      #fill.missing( data, mis.index, imputed )
    
    } # imputation loop
    # Check for convvergence
    Time.Elapsed <- proc.time( ) - ProcStart;
    if ( s > 5 || ( ( ( ( Time.Elapsed ) / 60 )[3] > 0.5 ) && s > 2 ) ) {
      con.check <- as.bugs.array( AveVar[ 1:s, , ] );
      if( max( con.check$summary[ ,8] ) < 1.1 )  { 
        converged.flg <- TRUE;
        if( !continue.on.convergence ) { 
          break;
        }
      }
      if( ( ( Time.Elapsed ) / 60 )[3] > max.minutes ) { 
        time.out.flg <- TRUE; 
        break;
      }
    }
    if( s==s_end ) { 
      max.iter.flg <- TRUE; 
    }
  } # iteration loop
  
  # Print out reason for termination
  cat(  if( converged.flg   ){
          "mi converged ("; 
        } else if( time.out.flg  ){
          "Time out, mi did not converge (";
        } else if( max.iter.flg ){
          "Reached the maximum iteration, mi did not converge (";
        } else{ 
          "Unknown termination (";
        }
      ,date(), ")\n" ) 
      
  # Automatic Preprocess
  if( preprocess ) {
    data <- mi.info.uncode( data, info );
  }
  # impute correlated variables
  for( cor.idx in 1:length( info ) ) {
    if( length( info[[cor.idx]]$correlated ) > 0 
         && info[[cor.idx]]$nmis > 0 
          && info[[cor.idx]]$include == FALSE ) {
      for ( ii in 1:n.imp ){
        mi.object[[ii]][[names(info)[[cor.idx]]]] <- do.call( mi.copy, 
                                                              args=list(
                                                                Y=org.data[[names(info)[cor.idx]]],
                                                                X=mi.data[[ii]][info[[cor.idx]]$determ.pred]));
      }
    }
  }
  
  mi <- new("mi", 
            call      = call,
            data      = org.data,
            m         = n.imp,
            mi.info   = info,
            imp       = mi.object,
            converged = converged.flg,
            coef.conv = as.bugs.array(strict.check(coef.val,s,n.imp)),
            bugs      = con.check);
################################################################
#  #Retro Grade residual codes
#  mi$check     <- exp.val;
#  mi$prd       <- prd.v al;
  return( mi );
}

##The simple imputation function
impute<- function ( a, a.impute ) { 
  return ( ifelse ( is.na ( a ), a.impute, a ) ) 
}

strict.check<-function(coefficient,n.iter,n.imp){
  res <- array(NA,c(n.iter,n.imp,0))
  for(i in 1:length(coefficient)){
    for(j in 1:dim(coefficient[[i]][[1]])[2]){
      res <-  array.append(res,matrix(unlist(lapply(coefficient[[i]], "[", , j)),,n.imp))
    }
  }
  return(res)
}

array.append<-function(a, b, d = 3){
  if(any(dim(a)[-d]!= dim(b)[-d])){
    print(dim(a))
    print(dim(b))
    stop(message="array dimention must be same for all the dimention except for the one that you are trying to append")
  } else{
    da <-  dim(a)
    da[d]<- ifelse(is.na( dim(a)[d]),1,dim(a)[d])+ ifelse(is.na( dim(b)[d]),1,dim(b)[d])
    ab <- c(a,b)
    dim(ab) = da
  }
  return(ab)
}


# ==============================================================================
# S4 print function for mi object
# ==============================================================================
setMethod("print", signature( x = "mi" ),
  function ( x, ... ) {
    n <- nrow(x@data)
    cat ( "\nMultiply imputed data set" );
    cat ( "\n\nCall:\n " );
    print( call.mi(x) );
    cat ( "\nNumber of multiple imputations: ", m(x),"\n");
    tab <- mi.info.table( info.mi(x) )[,c("names","type","number.mis")]
    tab <- data.frame(tab, proportion=tab[,"number.mis"]/n )
    cat ( "\nNumber and proportion of missing data per column:\n" );
    print ( tab );
    cat ( "\nTotal Cases:", n );
    r    <- 1 * is.na ( x@data );
    cat ( "\nMissing at least one item:", sum ( colSums(r)!= 0 ) );
    cat ( "\nComplete cases:", sum ( rowSums(r) == 0 ), "\n" );
    invisible( tab );
  }
)

# ==============================================================================
# S4 show function for mi object
# ==============================================================================
setMethod( "show", signature( object = "mi" ),
  function ( object ) {
    print( object );
  }
) 

# ==============================================================================
# S4 plot function for mi object
# ==============================================================================

setMethod( "plot", signature( x = "mi", y="missing" ),
  function ( x, m = 1, vrb = NULL, vrb.name = "Variable Score",
                        gray.scale = FALSE, mfrow=c( 1, 4 ), ... ) {
    if ( m(x) < m )  { 
      stop( message = paste( "Index of imputation 'm' must be within the range of 1 to", m(x) ) ) 
    } else{
      mids <- imp(x,m);
      Y    <- as.data.frame( x@data[ , names( mids ) ] );
      names( Y ) <- names( mids );
      par( mfrow = mfrow );
      for( i in 1:dim( Y )[2] ) {
        par( ask = TRUE );
        if( !is.null( mids[[i]] ) ) {
          plot( x = mids[[i]], y = Y[ ,names( mids )[i]], main = names( Y )[ i ] );
        }
      }
    }
  }
)

setMethod( "is.mi", signature( object = "mi" ),
  function ( object ){ 
    return(inherits ( object, "mi" )) 
  }
)

setMethod("call.mi", signature( object = "mi" ),
  function ( object ) { 
    return( object@call ) 
  }
)

setMethod("data.mi", signature( object = "mi" ),
  function ( object ) { 
    return( object@data ) 
  }
)

setMethod("converged", signature( object = "mi" ),
  function ( object ) { 
    return( object@converged ) 
  }
)

setMethod("m", signature( object = "mi" ),
  function ( object ) {
    return( object@m ) 
  }
)
setMethod("bugs.mi", signature( object = "mi" ),
  function ( object ){
    return( object@bugs ) 
  }
)

setMethod("info.mi", signature( object = "mi" ),
   function ( object ){
    return( object@mi.info ) 
  }
)

setMethod("imp", signature( object = "mi" ),
  function(object,m=1){
      return(object@imp[[m]])
  }
)

setMethod( "mi.completed", signature( object = "mi" ),
  function ( object, m = 1, outcome = c("data.frame","matrix") ) {
    outcome<- match.arg(outcome);
    if(outcome=="data.frame"){
      return(mi.data.frame(object,m));
    } else {
      return(mi.matrix(object,m));
    }
  }
)
setMethod( "mi.matrix", signature( object = "mi" ),
  function ( object, m = 1 ) {
    if( m(object) < m )  { stop( message = "Index of imputation is not within the range." ) }
    info <- info.mi(object)
    mis.name <- names( nmis(info)[ nmis( info ) > 0 & !all.missing( info ) ] );
    mimatrix <- data.mi(object);
    for ( i in 1:length(mis.name) ){
      nm <- mis.name[i]
      mimatrix[ ,nm] <- mi.imputed( imp(object,m)[[nm]],mimatrix[ ,nm] );
    }  
    mimatrix<-mapply(factor2num,mimatrix);
    return( as.matrix( mimatrix ) );
  }
)
setMethod( "mi.data.frame", signature( object = "mi" ),
  function ( object, m = 1 ) {
  if( !inherits ( object, "mi" ) ) { stop( message = "Object must be 'mi' class." ) }
  if( m(object)< m )  { stop( message = "Index of imputation is not within the range." ) }
  info <- info.mi(object)
  mis.name <- names( nmis(info)[ nmis( info ) > 0 & !all.missing( info ) ] );  
  mimatrix <- data.mi(object);
  for ( i in 1:length(mis.name) ){
      nm <- mis.name[i]
      mimatrix[ ,nm] <- mi.imputed( imp(object,m)[[nm]],mimatrix[ ,nm] );
  }  
  return( data.frame( mimatrix ) )
}
)
