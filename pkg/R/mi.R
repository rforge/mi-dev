#==============================================================================
# mi main function
#==============================================================================

mi <- function ( object, info, type = NULL, n.imp = 3, n.iter = 30, 
                  max.minutes = 20, rand.imp.method = "bootstrap", 
                  preprocess = FALSE, continue.on.convergence = FALSE,
                  seed = NA, check.coef.convergence = FALSE, 
                  add.priors = prior.control()) {
  
  call <- match.call( )                         # call
  if(!is.na(seed)){
    set.seed(seed) 
  }    # set random seed
  
  if(n.iter <=5){ 
    stop(message="number of iteration must be more than 5")
  }
  ProcStart     <- proc.time()                  # starting time
  
  # variable initialization
  time.out.flg  <- FALSE
  converged.flg <- FALSE
  max.iter.flg  <- FALSE
  Time.Elapsed  <- 0
  con.check     <- NULL
  coef.conv.check <- NULL

  if(class(object) %in% c("matrix", "data.frame")){ 
  # For data frame and matrix  
    object     <- data.frame(object)
    nameD      <- deparse(substitute(object))
    org.data   <- object
    data       <- object
    col.mis    <- !complete.cases(t(data)) 
    ncol.mis   <- sum(col.mis)
    if(missing(info)) {     
      info <- mi.info(data)    # create mi.info
    }      
    AveVar  <- array(NA, c(n.iter, n.imp, dim(data[,info$include])[2]*2))
    s_start <- 1
    s_end   <- n.iter
  } 
  else if (class(object) == "mi") {
  # for mi object
    org.data  <- data.mi(object)
    data      <- data.mi(object)
    col.mis   <- !complete.cases(t(data))
    ncol.mis  <- sum(col.mis)
    n.imp     <- m(object)
    info      <- info.mi(object)
    prev.iter <- dim(bugs.mi(object)$sims.array)[1]
    AveVar    <- array(NA, c(prev.iter + n.iter, n.imp, sum(.include(info))*2))
    coef.conv.check <- object@coef.conv$sims.array
    AveVar[ 1:prev.iter , , ]<- bugs.mi(object)$sims.array
    s_start <- prev.iter + 1
    s_end   <- prev.iter + n.iter
    add.priors$K <- 0
  } 
  else {
  # unexpected object
    stop( gettextf( "object class '%s' is not acceptable", class( object ) ) )
  }
  
  mis.index <-  apply(data, 2, is.na)

#  # Automatic Preprocess
#
#  if( preprocess ) {
#    data <- mi.info.recode(data, info)
#  }
 
  data <- data[,.include(info)]
  dimnames( AveVar ) <- list( NULL, NULL, 
                              c( paste( "mean(", colnames( data ),")",sep="" ), 
                                 paste( "sd(", colnames( data ), ")", sep="" ) ) )
  VarName.tm <- names(info)[.include(info) & .nmis(info)>0 ]
  VarName    <- VarName.tm[order(.imp.order( info )[.include(info) & .nmis(info)>0 ])]
  length.list<- sum( .include(info) & .nmis(info) >0 )
  
  # list initialization
  mi.data       <- vector("list", n.imp)
  start.val     <- vector("list", n.imp)
  mi.object     <- vector("list", n.imp)
  for (j in 1:n.imp){ 
    mi.data[[j]]  <-  if(class(object) %in% "mi" ){ 
                        data.frame(mi.matrix(object, m=j)[,.include(info)]) 
                      } 
                      else{ 
                        random.imp(data, method = rand.imp.method )
                      }
    start.val[[j]]<- vector( "list", length.list )
    mi.object[[j]]<- vector( "list", ncol.mis )
    names(mi.object[[j]]) <- names( info )[ .nmis(info)>0 ]
  }
  coef.val <- vector("list",ncol.mis)
  names(coef.val) <- names( info )[ .nmis(info)>0 ]
  for (jjj in 1:ncol.mis){
    coef.val[[jjj]] <- vector("list", n.imp)
  }
  names(mi.object)<- paste( "Imputation", 1:n.imp, sep="" )

  cat( "Beginning Multiple Imputation (", date(), "):\n" )
  # iteration loop
  for ( s in s_start:s_end ) {
    cat( "Iteration", s,"\n" )
    # imputation loop
    for ( i in 1:n.imp ){
      cat( " Imputation", i,  ": " )
      # variable loop
      for( jj in 1:length(VarName) ) {
        CurrentVar <- VarName[jj]
        
        # probability of cooling 
        prob.add.prior <- add.priors$K/s
        prob.add.prior <- ifelse(prob.add.prior > 1, 1, prob.add.prior)
        q <- rbinom(1, 1, prob=prob.add.prior) 
          
        if(q){
          cat(paste(CurrentVar, "*", sep=""), " ")
        }
        if(add.priors$augment.data){
          cat(paste(CurrentVar, "*", sep=""), " ")
        }
        else{
          cat( CurrentVar, "  " )
        }
        
        CurVarFlg <- ( names ( data ) == CurrentVar )
        dat <- data.frame( data[ ,CurVarFlg, drop=FALSE ], 
                            mi.data[[i]][ ,!CurVarFlg ] )
        
        if(add.priors$augment.data){
          pct.aug <- add.priors$pct.aug
          n.aug <- trunc((dim(data)[1]*pct.aug))
          dat <- rbind.data.frame(dat, .randdraw(data, n=n.aug))
        }
        
        names(dat) <- c( CurrentVar, names( data[,!CurVarFlg, drop=FALSE] ) )
        model.type   <- as.character( type.models( info[[CurrentVar]]$type ) )
        
        # Error Handling
        .Internal(seterrmessage(""))
        errormessage <- paste("\nError while imputing variable:", CurrentVar, ", model:",model.type,"\n")
        on.exit(cat(errormessage,geterrmessage()))
        on.exit(options(show.error.messages = TRUE),add = TRUE)
        options(show.error.messages = FALSE)
        
        # Error Handling
        mi.object[[i]][[CurrentVar]] <- with(data=dat, 
                                          do.call( model.type,
                                                    args = c( list( formula = info[[CurrentVar]]$imp.formula, 
                                                    data = dat,
                                          start=if(!is.null(start.val[[i]][[jj]])){
                                                  start.val[[i]][[jj]]
                                                }
                                                else{
                                                  NULL
                                                }),
                                          info[[CurrentVar]]$params
                              )))
        # Error Handling
        on.exit ()
        options( show.error.messages = TRUE )
        # Error Handling

        if(q){
          n.mis <- sum(is.na(dat[[CurrentVar]]))
          mi.object[[i]][[CurrentVar]]@random <- sample(na.exclude(dat[[CurrentVar]]), n.mis, replace=TRUE)
        }
        
        mi.data[[i]][[CurrentVar]][is.na( data[[CurrentVar]] )] <- mi.object[[i]][[CurrentVar]]@random
        data.tmp <<- mi.data
        coef.val[[CurrentVar]][[i]] <- rbind(coef.val[[CurrentVar]][[i]],coef(mi.object[[i]][[CurrentVar]]))
        start.val[[i]][[jj]] <- coef(mi.object[[i]][[CurrentVar]])
      } ## variable loop 
      cat("\n" )
      
      foo1 <- function(v){
        mean(unclass(v), na.rm=TRUE)
      }
      foo2 <- function(v){
        sd(unclass(v), na.rm=TRUE) 
      }
      AveVar[s,i,] <- c(sapply(mi.data[[i]], FUN = foo1),
                        sapply(mi.data[[i]], FUN = foo2))                        
    
    } # imputation loop
    # Check for convvergence
    Time.Elapsed <- proc.time( ) - ProcStart
    if (s > 5 || ((((Time.Elapsed)/60)[3] > 0.5) && s > 2)){
      con.check <- as.bugs.array(AveVar[1:s, , ])
      if(max(con.check$summary[ ,8]) < 1.1) { 
        converged.flg <- TRUE
        if(!continue.on.convergence){ 
          break
        }
      }
      if(((Time.Elapsed)/60)[3] > max.minutes){ 
        time.out.flg <- TRUE 
        break
      }
    }
    if(s==s_end) { 
      max.iter.flg <- TRUE 
    }
  } # iteration loop
  
  # Print out reason for termination
  cat(if(converged.flg ){
        "mi converged (" 
      } 
      else if( time.out.flg  ){
        "Time out, mi did not converge ("
      } 
      else if( max.iter.flg ){
        "Reached the maximum iteration, mi did not converge ("
      } 
      else{ 
        "Unknown termination ("
      }
      ,date(), ")\n"
      ) 
      
#  # Automatic Preprocess
#  if( preprocess ) {
#    data <- mi.info.uncode(data, info)
#  }
  # impute correlated variables
  for( cor.idx in 1:length(info)) {
    if( length(info[[cor.idx]]$correlated) > 0 
         && info[[cor.idx]]$nmis > 0 
          && info[[cor.idx]]$include == FALSE ) {
      for ( ii in 1:n.imp ){
        mi.object[[ii]][[names(info)[[cor.idx]]]] <- do.call( mi.copy, 
                                                              args=list(
                                                                Y=org.data[[names(info)[cor.idx]]],
                                                                X=mi.data[[ii]][info[[cor.idx]]$determ.pred]))
      }
    }
  }
  if(is.null(coef.conv.check)){
    coef.conv.check <- as.bugs.array(strict.check(coef.val,dim(coef.val[[1]][[1]])[1],n.imp))
  }
  else{
    tmp <- abind(coef.conv.check,strict.check(coef.val,dim(coef.val[[1]][[1]])[1],n.imp),along=1)
    coef.conv.check <- as.bugs.array(tmp)
  }
  
   mi <- new("mi", 
            call      = call,
            data      = org.data,
            m         = n.imp,
            mi.info   = info,
            imp       = mi.object,
            converged = converged.flg,
            coef.conv = coef.conv.check,
            bugs      = con.check)
  with(globalenv(), rm(data.tmp))
  return(mi)
}

##The simple imputation function
impute <- function ( a, a.impute ) { 
  out <- ifelse (is.na(a), a.impute, a)
  return (out) 
}

strict.check <- function(coefficient,n.iter,n.imp){
  res <- array(NA,c(n.iter,n.imp,0))
  for(i in 1:length(coefficient)){
    for(j in 1:dim(coefficient[[i]][[1]])[2]){
     res <- abind(res,matrix(unlist(lapply(coefficient[[i]], "[", , j)),,n.imp),along=3)
    }
  }
  return(res)
}


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
  function(object, m=1){
      return(object@imp[[m]])
  }
)
