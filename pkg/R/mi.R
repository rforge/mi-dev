#==============================================================================
# mi main function
#==============================================================================

mi <- function ( object, info, type = NULL, n.imp = 3, n.iter = 30, 
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                   preprocess = FALSE, continue.on.convergence = FALSE,
                    seed = NA ) {
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
    org.data  <- object$data;
    data      <- object$data;
    col.mis   <- !complete.cases( t( data ) );
    ncol.mis  <- sum( col.mis );
    n.imp     <- object$m;
    info      <- object$mi.info;
    prev.iter <- dim(object$bugs$sims.array)[1]
    AveVar    <- array( NA, c( prev.iter + n.iter,
                                 n.imp, 
                                  sum( include( object$mi.info ) ) * 2 ) );
    AveVar[ 1:prev.iter , , ]<- object$bugs$sims.array;
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
                              ) ) )
        # Error Handling
        on.exit ();
        options( show.error.messages = TRUE );
        # Error Handling
################################################################
#        #Retro Grade residual codes
#        prd.val[[i]][[s]][[CurrentVar]]<- mi.data[[i]][[CurrentVar]]
#        exp.val[[i]][[s]][[CurrentVar]]<- mi.object[[i]][[CurrentVar]]$expected

        mi.data[[i]][[CurrentVar]][is.na( data[[CurrentVar]] )] <- mi.object[[i]][[CurrentVar]]$random
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
  ###############################
  mi           <- list( call = NULL, data = NULL, n.imp = NULL, 
                        type = NULL, nmis = NULL, imp = NULL, 
                        converged = NULL, bugs = NULL);
################################################################
#  #Retro Grade residual codes
#                        ,check = NULL, prd =NULL
                        
  mi$call      <- call;
  mi$data      <- org.data;
  mi$m         <- n.imp;
  mi$mi.info   <- info;
  mi$nmis      <- colSums( 1*is.na( data ) );
  mi$imp       <- mi.object;
  mi$converged <- converged.flg;
  mi$bugs      <- con.check;
################################################################
#  #Retro Grade residual codes
#  mi$check     <- exp.val;
#  mi$prd       <- prd.val;
  class ( mi ) <- c( "mi");
  return( mi );
}

#mi.filehash <- function ( data, filename = NULL, type = NULL, m = 3, n.iter = 30, max.minutes = 20, seed = NA, rand.imp.method = "bootstrap" ) {
#    ProcStart <- proc.time()
#    time.out  <- FALSE
#    converged <- FALSE
#    con.check <- NULL
#    max.minutes<-max.minutes
#    call <- match.call( )
##    if( any( apply( data,2,"is.character" ) ) ) {
##        stop( message = paste( "variable must be numeric: variable '", names( data )[apply( data, 2,"is.character" )], "' is character\n\t", sep=""))
##    }
#    if( !is.na ( seed ) ) { set.seed( seed ) }           
#    col.mis    <- !complete.cases( t( data ) ) 
#    #col.mis    <- number.of.missing(data)
#    ncol.mis   <- sum( col.mis )
#    VarModel   <- if( !is.null( filename ) ) { read.models( filename = filename ) } 
#                    else { mi.models( data = data, model.print = FALSE, type.list = type ) }
#    VarType    <- attr  ( VarModel, "type" )
#    VarName    <- names ( VarModel )
#    VarName    <- VarName
#    length.list<- length( VarModel )
#    AveVar <- array( NA, c( n.iter, m, dim( data )[2] * 2 ) )
#    dimnames( AveVar ) <- list( NULL, NULL, c( paste( "mean(", colnames( data ),")",sep="" ), paste( "sd(", colnames( data ), ")", sep="" ) ) )
#    mi.data   <- vector( "list", m )
#    start.val <- vector( "list", m )
#    mi.object <- vector( "list", m )
#    for (j in 1:m){ 
#        mi.data[[j]]  <- random.imp( data, method = rand.imp.method ) 
#        start.val[[j]]<- vector( "list", length.list )
#        mi.object[[j]]<- list( )
#    }
#    dbCreate("mi.object") 
#    db1 <- dbInit("mi.object")
#    dbInsert(db1, "mi.object", mi.object)
#    #db1<-dumpObjects(mi.object,dbName="mi.object")
#    cat( "Beginning Multiple Imputation (",date(),"):\n")  
#    for ( s in 1:n.iter) {
#        cat( "Iteration", s,"\n" )
#        for ( i in 1:m ){
#            cat( " Imputation", i,  ": " )
#            for( jj in 1:ncol.mis ) {
#                cat( VarName[jj], " " )
#                dat <- data.frame( data[ ,names ( data ) == VarName[jj],FALSE ], mi.data[[i]][ ,names ( data ) != VarName[jj] ] )
#                names( dat ) <- c( VarName[jj], names( data[,names(data) != VarName[jj],FALSE] ) )
#                m.t <-  if ( !is.null( start.val[[i]][[jj]] ) ) { 
#                            paste( substr( VarModel[[VarName[jj]]] , 1, nchar( VarModel[[VarName[jj]]] )-1)
#                            , start.val[[i]][[jj]]
#                            , ")") 
#                        } 
#                        else{ 
#                            VarModel[[VarName[jj]]] 
#                        }
#                #m.t <- VarModel[[VarName[jj]]] 
#                #write.csv(mi.data[[i]],file=paste("iterafter",s,"-",i,"-",VarName[jj],".csv",sep=""))
#                #print(m.t)
#
#                  db1$mi.object[[i]][[jj]] <- eval( parse( text = paste( "with( dat, ",m.t, ")" ) ) )
##                mi.object[[i]][[jj]] <- try(eval( parse( text = paste( "with( dat, ",m.t, ")" ) ) ),TRUE)
##                if( inherits( mi.object[[i]][[jj]],"try-error" ) ){
##                    stop(message=paste("\n error occured while executing:", m.t, "\n",mi.object[[i]][[jj]]))
##                }
#                #mi.object[[i]][[jj]] <- tryCatch(eval( parse( text = paste( "with( dat, ",m.t, ")" ) ) ),error=cat("error while processing",VarName[jj]),finally=cat("Exiting"))
#                 mi.data[[i]][[VarName[jj]]][is.na(data[[VarName[jj]]])] <- db1$mi.object[[i]][[jj]]$random
#                start.val[[i]][[jj]] <- mi.start(  db1$mi.object[[i]][[jj]] )
#            } ## variable loop 
#            cat("\n" )   
#            AveVar[s,i,] <- c( mean( mi.data[[i]] ),sd( mi.data[[i]] ) )
#        }
#        dbReorganize(db1)
#        db1<-dbInit("mi.object")
#        if ( s > 5 || ( ( ( ( proc.time( ) - ProcStart ) / 60 )[3] > 0.5 ) && s > 2 ) ) {
#            con.check <- as.bugs.array( AveVar[1:s,,] )
#            if( max( con.check$summary[ ,8] ) < 1.1 )  { converged <- TRUE; break }
#            if( ( ( proc.time( ) - ProcStart ) / 60 )[3] > max.minutes ) { time.out <- TRUE; break }
#        }
#                
#    }
#    names(db1$mi.object)<- paste( "Imputation", 1:m, sep="" )
#    for( v in 1:m){
#        names( db1$mi.object[[v]] ) <- VarName
#    }
#    cat( if( converged   ){"mi converged (" }
#         else if( time.out  ){"Time out, mi did not converge (" }
#         else if( s==n.iter ){"Reached the maximum iteration, mi did not converge ("}
#         else{ "Unknown termination ("}
#        ,date(), ")\n" ) 
#    mi           <- list( call = NULL, data = NULL, m = NULL, type = NULL, nmis = NULL, imp = NULL, converged = NULL, bugs = NULL )
#    mi$call      <- call
#    mi$data      <- data
#    mi$m         <- m
#    mi$type      <- VarType
#    mi$nmis      <- colSums( 1*is.na( data ) )
#    #mi$imp       <- list.mi
#    mi$imp       <- db1$mi.object
#    mi$converged <- converged
#    mi$bugs      <- con.check
#    class ( mi ) <- c( "mi", "list" )
#    return( mi )
#}

## ========================================================================
## read model from text
## ========================================================================
#read.models <- function ( filename ) {
#    if ( missing ( filename ) ) { stop ( message = "'filename' is unspecified." ) }
#    else {
#        model.list <- dget ( filename ) 
#        cat( paste ( "Read file: ", getwd(), "/", filename, "\n", sep="" ) )
#        return ( model.list )
#    }
#}

##The simple imputation function
impute<- function ( a, a.impute ) { 
  return ( ifelse ( is.na ( a ), a.impute, a ) ) 
}
is.mi <- function ( object ){ 
  return(inherits ( object, "mi" )) 
}
call.mi <- function ( object ) { 
  return( object$call ) 
}
data.mi <- function ( object ) { 
  return( object$data ) 
}
converged.mi <- function ( object ) { 
  return( object$converged ) 
}
m.mi <- function ( object ) {
  return( object$m ) 
}
bugs.mi <- function ( object ){
  return( object$bugs ) 
}
info.mi <- function ( object ){
  return( object$mi.info ) 
}

imp.mi <-function(object,m=1){
    return(object$imp[[m]])
}


# ========================================================================
# number of missing data per variable 
# ========================================================================
number.of.missing <- function( data ) {
  if( inherits( data, "matrix" ) ) { 
    resultVec <- !complete.cases( t( data ) ) 
  } else if( inherits( data, "filehashDB1" ) ) {
    nameDB   <- dbList( db )
    resultVec<- vector( "integer", length( nameDB ) )
    names( resultVec ) <- nameDB
    for (i in 1:length( nameDB ) ) {
      resultVec[i] <- sum( is.na( db[[nameDB[i]]] ) )
    }
  } else{
    resultVec <-FALSE
  }
  return( resultVec )
}

    
fill.missing <- function ( data, mis.index, imputed ){
  data[mis.index] <- imputed
  return(data)
}
