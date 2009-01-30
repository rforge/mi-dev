mi.check.correlation <- function ( data, threshhold = 1 ){
  #cat("Checking for correlation")
  cor.data <- cor(data, use="pairwise.complete.obs")
  diag(cor.data) <- 1
  #index<-abs( cor.data*upper.tri(cor.data)) >= threshhold  
  index <- abs(cor.data - diag(dim(cor.data)[1])) >= threshhold 
  result <- vector("list", dim(index)[1])
  for(i in 1:dim(index)[1]){
    result[[i]] <- c(names(which(index[i,]==1)))
  }
  return(result)
}

# preprocess: this is ugly..but right..need to improve it
mi.preprocess <- function(data, varnames = NULL, trans = NULL){
  n.col <- ncol(data)
  n.row <- nrow(data)
  var.name <- names(data)
  if(!is.null(varnames)&!is.null(trans)){
    if(length(trans)==1){
      trans <- rep(trans, length(varnames))
    }
    if (length(varnames) != length(trans)){
      warning(message="the length of the selected variable names does not equal to 
      the length of the transforming functions.
      The process will use the first element of the transforming function!")
      trans <- rep(trans[1], length(varnames))
    }
  }    
  if(is.null(varnames)){
    idx <- NULL
    TMP <- NULL
    if (is.null(trans)){
      trans = "log"
    }
    for (i in 1:n.col){
      typ <- typecast(data[,i])
      tmp <- NULL
      if (typ == "mixed"){
        if(as.numeric(trans == "sqrt")){
          Sqrt <- sqrt(data[,i])
          Sqrt.lab <- paste(var.name[i], "sqrt", sep=".")
          tmp <- cbind(tmp, Sqrt)
          dimnames(tmp)[[2]] <- Sqrt.lab
        }
        else{
          Ind <- ifelse(data[,i] > 0, 1, ifelse(data[,i]==0, 0, NA))
          Log <- log(ifelse(data[,i] > 0, data[,i], NA))
          Ind.lab <- paste(var.name[i], "ind", sep=".")
          Log.lab <- paste(var.name[i], "log", sep=".")
          tmp <- cbind(tmp, Ind, Log)
          dimnames(tmp)[[2]] <- c(Ind.lab, Log.lab)
        }
        TMP <- cbind(TMP, tmp)
        idx <- c(idx, i)
      }
      if (typ == "positive-continuous"){
        if(trans == "sqrt"){
          Sqrt <- sqrt(data[,i])
          Sqrt.lab <- paste(var.name[i], "sqrt", sep=".")
          tmp <- cbind(tmp, Sqrt)
          dimnames(tmp)[[2]] <- Sqrt.lab
        }
        else{
          Log <- log(data[,i])
          Log.lab <- paste("log", var.name[i], sep=".")
          tmp <- cbind(tmp, Log)
          dimnames(tmp)[[2]] <- Log.lab
        }
        TMP <- cbind(TMP, tmp)
        idx <- c(idx, i)
      }
    }
    var.name <- var.name[-idx]
    data <- data[,-idx]
    data <- cbind(data, TMP)
    data <- as.data.frame(data)
  }
  else{
    idx <- pmatch(varnames, var.name)
    TMP <- NULL
    if (is.null(trans)){
      trans = "log"
    }
    else{
      trans[idx] <- trans
    }
    for (i in idx){
      typ <- typecast(data[,i])
      tmp <- NULL
      if (typ == "mixed"){
        if(trans[i]=="sqrt"){
          Sqrt <- sqrt(data[,i])
          Sqrt.lab <- paste(var.name[i], "sqrt", sep=".")
          tmp <- cbind(tmp, Sqrt)
          dimnames(tmp)[[2]] <- Sqrt.lab
          TMP <- cbind(TMP, tmp)
          idx <- c(idx, i)        
        }
        else{
          Ind <- ifelse(data[,i] > 0, 1, ifelse(data[,i]==0, 0, NA))
          Log <- log(ifelse(data[,i] > 0, data[,i], NA))
          Ind.lab <- paste(var.name[i], "ind", sep=".")
          Log.lab <- paste(var.name[i], "log", sep=".")
          tmp <- cbind(tmp, Ind, Log)
          dimnames(tmp)[[2]] <- c(Ind.lab, Log.lab)
          TMP <- cbind(TMP, tmp)
          idx <- c(idx, i)
        }
      }
      if (typ == "positive-continuous"){
        if(trans[i]=="sqrt"){
          Sqrt <- sqrt(data[,i])
          Sqrt.lab <- paste(var.name[i], "sqrt", sep=".")
          tmp <- cbind(tmp, Sqrt)
          dimnames(tmp)[[2]] <- Sqrt.lab
          TMP <- cbind(TMP, tmp)
          idx <- c(idx, i)        
        }
        else{
          Log <- log(data[,i])
          Log.lab <- paste("log", var.name[i], sep=".")
          tmp <- cbind(tmp, Log)
          dimnames(tmp)[[2]] <- Log.lab
          TMP <- cbind(TMP, tmp)
          idx <- c(idx, i)
        }
      }
    }
    var.name <- var.name[-idx]
    data <- data[,-idx]
    data <- cbind(data, TMP)
    data <- as.data.frame(data)
  }
}

mi.postprocess <- function(trans.data){
  n.chains <- length(trans.data)
  var.name <- names(trans.data[[1]])
  chk1 <- sum(grep(".ind", var.name))
  chk2 <- sum(grep(".log", var.name))
  chk3 <- sum(grep("log.", var.name))
  chk4 <- sum(grep(".sqrt", var.name))
  if(chk1 > 0){
    var.name <- names(trans.data[[1]])
    idx1 <- grep(".ind", var.name)
    idx2 <- grep(".log", var.name)
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
  }
  if(chk3 > 0){
    var.name <- names(trans.data[[1]])
    idx1 <- grep("log.", var.name)
    var.name1 <- var.name[idx1]
    var.name1 <- gsub("log.", "", var.name1)
    var.name <- c(var.name1, var.name[-idx1])
    for (s in 1:n.chains){
      data <- exp(trans.data[[s]][,idx1])
      trans.data[[s]] <- trans.data[[s]][,-idx1]
      trans.data[[s]] <- cbind.data.frame(data, trans.data[[s]])
      names(trans.data[[s]]) <- var.name
    }  
  }
  if(chk4 > 0){
    var.name <- names(trans.data[[1]])
    idx1 <- grep(".sqrt", var.name)
    var.name1 <- var.name[idx1]
    var.name1 <- gsub(".sqrt", "", var.name1)
    var.name <- c(var.name1, var.name[-idx1])
    for (s in 1:n.chains){
      data <- (trans.data[[s]][,idx1])^2
      trans.data[[s]] <- trans.data[[s]][,-idx1]
      trans.data[[s]] <- cbind.data.frame(data, trans.data[[s]])
      names(trans.data[[s]]) <- var.name
    }  
  } 
  return(trans.data)
}







#
#mi.preprocess <- function( data, info=NULL, trans = trans.func, name = trans.name ){
#  if(is.null(info)){ 
#    info <- mi.info( data ) 
#  }
#  inVar     <- .include( info )
#  type.list <- type( info )
#  processed <-data[ ,inVar,drop=FALSE]
#  
#  
#  for ( i in 1:dim( processed )[2]){
#    processed[,i] <- trans( type.list[i])( data[,i] )
#    if( !is.null( name( type.list[i] ) ) ) {
#      dimnames( processed )[[2]][i] <- paste( name( type.list[i] ),dimnames( data )[[2]][i], sep="" )
#    }
#  }
#  return( processed )
#}
#
#
#
#mi.postprocess<-function(data, type.list, trans = inverse.func, name = trans.name  ){
#  processed <-data
#  for (i in 1:dim(data)[2]){
#    processed[,i] <- trans(type.list[i])(data[,i])
#
#    if(!is.null(name(type.list[i]))){
#      dimnames(processed)[[2]][i]<-sub(name(type.list[i]),"",dimnames(data)[[2]][i])
#    }
#  }
#  return(processed)
#}

#mi.recode <- function(data, info, undo=FALSE){
#  for (i in 1:dim(data)[2]){
#    if(is.character(data[,i])){
#      if(undo){
#        rec<- paste("'", .level(info)[[i]],"' =",names(.level(info)[[i]]),sep="")
#      }
#      else{
#        rec<-paste("'",names(.level(info)[[i]]),"' =",.level(info)[[i]],sep="")
#      }
#      for(j in 1:length(rec)){
#        data[,i]<-recode(data[,i],rec[i])
#      }
#    }
#    else if (is.factor(data[,i])){
#      if(undo){
#        levels(data[,i]) <- names(.level(info)[[i]])
#      }
#      else{
#        levels(data[,i]) <- .level(info)[[i]]
#      }
#    }
#  }
#  return(data)
#}
#
#trans.func<-function(type){
#    fun <-if     (type == "squareroot-continuous" ) {sqrt}
#          else if(type == "logscale-continuous" ) {log}
#          else if(type == "mixed" ) {function ( x ) {
#                                        x[x>0&!is.na(x)]<-sqrt(x[x>0&!is.na(x)])  
#                                        return(x)
#                                      } }
#          else { function ( x ) { x } }
#    return( fun )
#}
#trans.name<-function(type){
#    name <-if     (type == "squareroot-continuous" ) {"sqrt."}
#          else if(type == "logscale-continuous" ) {"log."}
#          else if(type == "mixed" ) {"msqrt."}
#          else { NULL }
#    return( name )
#}
#
#inverse.func<-function(type){
#    fun <-if     (type == "squareroot-continuous" ) {function ( x ) { x^2 }}
#          else if(type == "logscale-continuous" ) {function ( x ) { exp( x ) }}
#          else if(type == "mixed" ) {function ( x ) {
#                                        x[ x>0 & !is.na( x ) ] <- ( x[ x>0 & !is.na( x ) ] )^2 
#                                        return( x )
#                                      } }
#          else { function ( x ) { x } }
#    return( fun )
#}
#
#


#plot.outcheck <- function( data , standardize = FALSE ) {
#  par( mar = c( 4.5, 11, 3, 1 ) )
#  nm <- names( data )
#  if( standardize ) {
#    data <- apply( data, 2,function(x){(x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))})
#  }
#  mn <- colMeans(data, na.rm=T)
#  se <- apply( data, 2, sd, na.rm=TRUE)
#  ot <- apply( data, 2, function(x){boxplot(x,plot=FALSE)$out})
#  #co <- apply( data, 2, function(x){boxplot(x,plot=FALSE)$conf})
#  plot(range(data,na.rm=T),c(0,dim(data)[2]+1),type="n",yaxt = "n",ylab="",xlab="",yaxs = "i")
#  axis(side=2,at=1:dim(data)[2],labels=nm,las=2)
#  for(i in 1:dim(data)[2]){
#    points(mn[i], i)
#    lines( c( mn[i]+se[i], mn[i]-se[i] ), c( i, i ) )
#    #lines( c( co[1,i],co[2,i] ), c( i, i ) )
#    points( ot[[i]], ot[[i]]*0+i, pch=20 )
#  }
#  return(ot)
#}


#mi.check.discrete <- function (data, threshhold = 0.5){
#  discrete<-length(unique(data)) < length(data)*threshhold
#  return(discrete)
#}


#mi.remove.all.missing <- function ( data ){
#  cat("following variables have no observed values, thus will be omitted from the imputation procedure\n")
#  print(dimnames(data)[[2]][colSums(is.na(data))==dim(data)[[1]]])
#  return(data[colSums(is.na(data))!=dim(data)[[1]]])
#}






#mi.check <- function ( data , filename = "model.txt" ) {
#  n.col <- ncol(data)
#  varnames <- names(data)
#  result.type <- vector("character", n.col)
#  names(result.type) <- varnames
#  default.type <- typecast(data)
#  cat("-----starting the check (select 0 to quit)-------\n")
#  res0 <- menu(
#    c("go through all of the variables.",
#      "choose specific variable to change."),
#      title="Would you like to"
#      )
#  # Go through all the variables
#  if(res0 == 1 ) {
#    for(i in 1:dim(data)[2]){
#      y <- data[,i]
#      y.name <- varnames[i]
#      cat("checking variable:", y.name,"\n")
#      type <- default.type[i]
#      cat("default type for variable:", y.name, "is", type, "\n")
#      
#      res <- menu(c("Yes","No"), title="would you like to change it?")
#      if(res==1){
#        new.type <- menu(mi.types(),title="select the type:")
#        result.type[i] <- if(new.type==0){ 
#                            break 
#                          } 
#                          else{ 
#                            new.type 
#                          }
#      }
#      else if(res==2){
#        result.type[i] <- type
#      }
#      else if(res==0){
#        break
#      }
#      else{
#      }    
#    }
#  }
#  # Choose variable
#  else if (res0 == 2){
#    esc<-0
#    while( esc == 0 ){
#      cat("Please enter a name of variable you wish to edit\n")
#      print(paste(varnames, sep=","))
#      varname <- scan( what = character(), sep = "\n", strip.white = TRUE, nlines=1, quiet=TRUE )
#      if( varname %in% dimnames(data)[[2]]){
#                
#      }
#      else if ( varname == "list"){
#       
#      }
#      else if ( varname == "end"){
#        esc <- "1"
#      }
#      else{}
#    }
#  }
#  
#  else if (res0 == 0){
#   break
#  }
#  return(result.type)
#}
