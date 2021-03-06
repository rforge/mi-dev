# ==============================================================================
# Histograms for the completed, observed and imputed values
# ==============================================================================
setMethod( "mi.hist", signature( object= "ANY", Yobs = "ANY"  ),
 function( object, Yobs,  b = seq( min( c(Yobs,Yimp),na.rm=TRUE ),
           max( c(Yobs,Yimp),na.rm=TRUE ),
           length.out = sqrt( length( Yimp ) ) ), gray.scale = FALSE,
           main = paste("Histogram of ", deparse( substitute( Yobs ) ) ),
           xlab = deparse( substitute( Yobs ) ), ylab = "Frequency",
           binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) ),
           obs.col = "blue", imp.col = "black", mis.col = "red",
           obs.lty = 1, imp.lty = 1, mis.lty = 1,
           obs.lwd = 1, imp.lwd = 1, mis.lwd = 1, mlt = 0.1, type, ... ){
    #print(cbind(Yobs,object))
  Yimp <- object
  if( length( Yobs ) != length( Yimp ) ){stop ( message = "observed and imputed vectors must be of same length" ) }
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if( missing(type)) type <- typecast( Yimp )
    #if( is.null( xlab ) ) { xlab <- deparse( substitute( Yobs ) ) }
  if( gray.scale == TRUE ) {
    obs.col <- gray( 0.6 )
    imp.col <- gray( 0.8 )
    mis.col <- gray( 0 )
    obs.lty <- 3
    imp.lty <- 1
    mis.lty <- 1
  }
    mis <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
    if( type == "nonnegative" ) {
        ipd   <- Yimp[ is.na( Yobs ) ]
        obs   <- Yobs[ !is.na( Yobs ) ]
        pz.ipd<- round( sum( ipd==0 ) / length( ipd ) * 100, 0 )
        z.ipd <- c( paste( "zeros=", pz.ipd, "%" ) )
        pz.obs<- round( sum( obs==0 ) / length( Yobs ) * 100, 0 ) #round(as.vector(table(Yobs[Yobs==0]))/length(Yobs.nomis)*100,0)
        z.obs <- c( paste( "zeros=", pz.obs, "%" ) )
        pz.imp<- round( sum( Yimp==0 )/ length( Yimp ) * 100, 0 ) #round(as.vector(table(imp[imp==0]))/length(imp)*100,0)
        z.imp <-c( paste( "zeros=", pz.imp, "%" ) )
        obs.nomis <- obs.nomis[ obs.nomis > 0 ]
        mis  <- mis[ mis > 0 ]
        Yimp <- Yimp[ Yimp > 0 ]
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis, plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        xrange <- range( c(obs[obs!=0],ipd[ipd!=0]),na.rm=TRUE )
        xadj  <- h.imp$breaks[2]- h.imp$breaks[1]
        xrange <- xrange + c(-xadj,+xadj)
        plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) * 1.05 ), yaxs = "i", xlab = xlab,
            xlim=xrange, ylab = ylab, type = "n", bty = "l", main = main )
        xb <- max(h.imp$breaks)-max(h.imp$breaks) * 0.2
        yb1 <- max( h.imp$counts ) - max( h.imp$counts ) * 0.1
        yb2 <- max( h.imp$counts ) - max( h.imp$counts ) * 0.3
        yb3 <- max( h.imp$counts ) - max( h.imp$counts ) * 0.5
        text( xb, yb1, z.imp, col=col[1],cex = .8 )
        text( xb, yb2, z.obs, col=col[2],cex = .8 )
        text( xb, yb3, z.ipd, col=col[3],cex = .8 )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        #histlineplot ( h.obs, shift=mlt*binwidth, col=col[2], lty = lty[2], lwd = lwd[2] )
        #histlineplot ( h.imp, col=col[1] , lty = lty[1], lwd = lwd[1] )

        axis( 1, tick = TRUE, col.axis = 'black' )
    }
    else if( type == "continuous" ) {
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis,  plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) * 1.05 ), yaxs = "i", xlab = xlab,
             xlim = range( Yimp ), ylab = ylab, type = "n", bty = "l", main = main )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        axis( 1, tick = TRUE, col.axis = 'black' )
    }
    else if( type == "binary" ) {
        Yimp <- Yimp
        b <-seq( 0, ceiling( max( Yimp ) ), 0.2 )
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis,  plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        plot( c(min(Yimp)-max(Yimp)*0.2,max(Yimp)+max(Yimp)*0.2), c( 0, max( h.imp$counts ) * 1.05 ), yaxs = "i", xlab = xlab, ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
        lab <- as.numeric( names( table( obs.nomis ) ) )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        axis(1, lab, tick = TRUE, col.axis = 'black')
    }
    else if( type == "ordered-categorical" ) {
        b <- seq( min(c(obs.nomis,mis,Yimp)), ceiling( max( Yimp ) ), 0.2 )
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis, plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        print(range( h.imp$breaks )+c(-1,0))
        plot( range( h.imp$breaks )+c(-1,0), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
        xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
        lab <- as.numeric( names( table( obs.nomis ) ) )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        axis( 1, lab, tick = TRUE, col.axis = 'black' )
    }
    else if( type == "unordered-categorical" ) {
        b <- seq( min(c(obs.nomis,mis,Yimp)), ceiling( max( Yimp ) ), 0.2 )
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis, plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
        xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
        lab <- as.numeric( names( table( obs.nomis ) ) )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        axis( 1, lab, tick = TRUE, col.axis = 'black' )
    }
    else if( type == "predictive-mean-matching" ) {
        #b <- seq( 0, ceiling( max( Yimp ) ), 0.2 )
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis, plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
        xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
        lab <- as.numeric( names( table( obs.nomis ) ) )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        axis( 1, lab, tick = TRUE, col.axis = 'black' )
    }
    else if( type == "norm" ) {
        #b <- seq( 0, ceiling( max( Yimp ) ), 0.2 )
        h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
        h.mis <- hist( mis, plot = FALSE, breaks = b )
        h.imp <- hist( Yimp, plot = FALSE, breaks = b )
        plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
        xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
        lab <- as.double( names( table( obs.nomis ) ) )
        if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
        histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
        histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
        histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
        axis( 1, tick = TRUE, col.axis = 'black' )
    }

}
)

setMethod( "mi.hist", signature( object = "mi.method", Yobs="ANY" ),
  function ( object,  Yobs, b = NULL, binwidth = NULL, gray.scale = FALSE,
              main = paste("Histogram of ", deparse( substitute( Yobs ) )),
              xlab = deparse( substitute( Yobs ) ), ylab = "Frequency",
              obs.col = "blue", imp.col = "black", mis.col = "red",
              obs.lty = 1, imp.lty = 1, mis.lty = 1,
              obs.lwd = 1, imp.lwd = 1, mis.lwd = 1, mlt = 0.1, ... )
{
  Yimp <- imputed( object, Yobs )
  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if( is.null( binwidth ) ) {
    binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )
  }
  if( is.null( b )) {
    b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )
  }
  if( gray.scale == TRUE ) {
    obs.col = gray( 0.6 )
    imp.col = gray( 0.8 )
    mis.col = gray( 0 )
    obs.lty = 3
    imp.lty = 1
    mis.lty = 1
  }
  h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
  h.mis <- hist( mis,  plot = FALSE, breaks = b )
  h.imp <- hist( Yimp, plot = FALSE, breaks = b )
  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) * 1.05 ),
        yaxs = "i", xlab = xlab,xaxt = "n",xlim = range( Yimp ),ylab = ylab,
        type = "n", bty = "l", main = main )
  axis( 1, tick = TRUE, col.axis = 'black' )
  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) { mlt<-0.2 }
  histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
  histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
  histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )

}
)

setMethod("mi.hist", signature( object = "mi.binary",Yobs = "ANY"),
 function (  object, Yobs,b = NULL, binwidth = NULL, gray.scale = FALSE,
            main = paste("Histogram of ", deparse( substitute( Yobs ) )),
            xlab = deparse( substitute( Yobs ) ), ylab = "Frequency",
            obs.col = "blue", imp.col = "black", mis.col = "red",
            obs.lty = 1, imp.lty = 1, mis.lty = 1,
            obs.lwd = 1, imp.lwd = 1, mis.lwd = 1, mlt = 0.1, ... )
 {
  Yimp <- .factor2num(imputed(object,Yobs))
  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if( is.null( binwidth ) ) { binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )}
  if( is.null( b )) { b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )}
  if( gray.scale == TRUE ) {
    obs.col = gray( 0.6 )
    imp.col = gray( 0.8 )
    mis.col = gray( 0 )
    obs.lty = 3
    imp.lty = 1
    mis.lty = 1
  }
  b <-seq( 0, ceiling( max( Yimp ) ), 0.2 )
  h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
  h.mis <- hist( mis,  plot = FALSE, breaks = b )
  h.imp <- hist( Yimp, plot = FALSE, breaks = b )
  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) * 1.05 ), yaxs = "i", xlab = xlab,
       xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
  lab <- as.double( names( table( obs.nomis ) ) )
  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
  histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
  histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
  histlineplot ( h.imp, col = imp.col, lty = imp.lty, lwd = imp.lwd )
  axis(1, lab, tick = TRUE, col.axis = 'black')
}
)
setMethod("mi.hist", signature( object = "mi.polr",Yobs = "ANY"),
 function ( object, Yobs,  b = NULL, binwidth = NULL, gray.scale = FALSE,
            main = paste("Histogram of ", deparse( substitute( Yobs ) )),
            xlab = deparse( substitute( Yobs ) ), ylab = "Frequency",
            obs.col = "blue", imp.col = "black", mis.col = "red",
            obs.lty = 1, imp.lty = 1, mis.lty = 1,
            obs.lwd = 1, imp.lwd = 1, mis.lwd = 1, mlt = 0.1, ... )
{
  Yimp <- as.numeric(imputed(object, Yobs))
  Yobs <- as.numeric(Yobs)
  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if(is.factor(Yobs)){
    maxlev <-as.numeric(max( levels(Yimp) ))
    minlev <-as.numeric(min( levels(Yimp) ))
    if( is.null( binwidth ) ) { binwidth = ( maxlev - minlev ) / sqrt( length( Yimp ) )}
    if( is.null( b )) { b <- seq( minlev, maxlev, length.out = sqrt( length( Yimp ) ) )}
    b <- seq( floor(minlev), ceiling( maxlev ), 0.2 )
  } else {
    if( is.null( binwidth ) ) { binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )}
    if( is.null( b )) { b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )}
    b <- seq( floor(min( Yimp ) ), ceiling( max( Yimp ) ), 0.2 )
  }

  if( gray.scale == TRUE ) {
    obs.col = gray( 0.6 )
    imp.col = gray( 0.8 )
    mis.col = gray( 0 )
    obs.lty = 3
    imp.lty = 1
    mis.lty = 1
  }
  h.obs <- hist( as.numeric(obs.nomis), plot = FALSE, breaks = b )
  h.mis <- hist( as.numeric(mis), plot = FALSE, breaks = b )
  h.imp <- hist( as.numeric(Yimp), plot = FALSE, breaks = b )
  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
  xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
  lab <- as.double( names( table( obs.nomis ) ) )
  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
  histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
  histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
  histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
  axis( 1, lab, tick = TRUE, col.axis = 'black' )
}
)


setMethod("mi.hist", signature(object = "mi.pmm",Yobs="ANY"),
 function ( object, Yobs, main = paste("Histogram of ", deparse( substitute( Yobs ) )),
             gray.scale = FALSE, xlab = deparse( substitute( Yobs ) ), ylab = "Frequency",
             b = NULL, binwidth = NULL, col = c( "black", "blue", "red" ),
             lty = c( 1, 1, 1 ), lwd = c( 1, 1, 1 ), mlt = 0.1, ... )
{
  Yimp <-imputed(object,Yobs)
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

setMethod("mi.hist", signature(object = "mi.categorical",Yobs="ANY"),
 function (  object, Yobs,b = NULL, binwidth = NULL, gray.scale = FALSE,
             main = paste("Histogram of ", deparse( substitute( Yobs ) )),
              xlab = deparse( substitute( Yobs ) ), ylab = "Frequency",
               obs.col = "blue", imp.col = "black", mis.col = "red",
              obs.lty = 1, imp.lty = 1, mis.lty = 1,
              obs.lwd = 1, imp.lwd = 1, mis.lwd = 1, mlt = 0.1, ... )
 {
  Yimp <- as.numeric(imputed(object,Yobs))
  Yobs <- as.numeric(Yobs)
  mis  <- Yimp[ is.na( Yobs ) ] ##the vector of the imputed values
  if( !is.null( is.na( Yobs ) ) ) { obs.nomis <- Yobs[ !is.na( Yobs ) ] }
  if( is.null( binwidth ) ) { binwidth = ( max( Yimp ) - min( Yimp ) ) / sqrt( length( Yimp ) )}
  if( is.null( b )) { b <- seq( min( Yimp ), max( Yimp ), length.out = sqrt( length( Yimp ) ) )}
  if( gray.scale == TRUE ) {
    obs.col = gray( 0.6 )
    imp.col = gray( 0.8 )
    mis.col = gray( 0 )
    obs.lty = 3
    imp.lty = 1
    mis.lty = 1
  }
  b <- seq( min(c(obs.nomis,mis,Yimp)), ceiling( max( Yimp ) ), 0.2 )
  h.obs <- hist( obs.nomis, plot = FALSE, breaks = b )
  h.mis <- hist( mis, plot = FALSE, breaks = b )
  h.imp <- hist( Yimp, plot = FALSE, breaks = b )
  plot( range( h.imp$breaks ), c( 0, max( h.imp$counts ) *1.05 ), yaxs = "i", xlab = xlab,
  xlim = range( Yimp ), ylab = ylab, xaxt = "n", tck = 0, type = "n", bty = "l", main = main )
  lab <- as.double( names( table( obs.nomis ) ) )
  if( max( c( h.obs$counts, h.mis$counts, h.imp$counts)) > 100) {mlt<-0.2}
  histlineplot ( h.mis, shift = -mlt*binwidth,
                  col = mis.col, lty = mis.lty, lwd = mis.lwd )
  histlineplot ( h.obs, shift = mlt*binwidth,
                  col = obs.col, lty = obs.lty, lwd = obs.lwd )
  histlineplot ( h.imp, col = imp.col , lty = imp.lty, lwd = imp.lwd )
  axis( 1, lab, tick = TRUE, col.axis = 'black' )
}
)

##The function for the histogram
histlineplot <- function ( h, shift = 0, col = "black", zero = TRUE,
                            lty = 1, lwd = 1, ... ) {
  n.bins <- length ( h$breaks ) - 1
  x.pos  <- h$breaks[ rep( c( 1, 2:n.bins, n.bins+1 ), c( 1, rep( 2, n.bins-1 ), 1 ) ) ]
  y.pos  <- rep ( h$counts, rep( 2, n.bins ) )
  if ( zero ) {
    x.pos <- c( x.pos[1], x.pos, x.pos[ length( x.pos ) ] )
    y.pos <- c( 0, y.pos, 0 )
  }
  lines ( x.pos + shift, y.pos, col = col, lty = lty, lwd = lwd )
}
