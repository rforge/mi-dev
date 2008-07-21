# ==============================================================================
# generic method definition 
# ==============================================================================

if ( !isGeneric( "is.mi" )) {
  setGeneric( "is.mi",
              function( object ){
                standardGeneric( "is.mi" )
              } 
  )
}
if ( !isGeneric( "call.mi" )) {
  setGeneric( "call.mi",
              function( object ){
                standardGeneric( "call.mi" )
              } 
  )
}
if ( !isGeneric( "data.mi" )) {
  setGeneric( "data.mi",
              function( object ){
                standardGeneric( "data.mi" )
              } 
  )
}
if ( !isGeneric( "converged" )) {
  setGeneric( "converged",
              function( object ){
                standardGeneric( "converged" )
              } 
  )
}
if ( !isGeneric( "m" )) {
  setGeneric( "m",
              function( object ){
                standardGeneric( "m" )
              } 
  )
}

if ( !isGeneric( "bugs.mi" )) {
  setGeneric( "bugs.mi",
              function( object ){
                standardGeneric( "bugs.mi" )
              } 
  )
}
if ( !isGeneric( "info" )) {
  setGeneric( "info",
              function( object ){
                standardGeneric( "info" )
              } 
  )
}

if ( !isGeneric( "imp" )) {
  setGeneric( "imp",
              function( object,... ){
                standardGeneric( "imp" )
              } 
  )
}

if ( !isGeneric( "mi.matrix" )) {
  setGeneric( "mi.matrix",
              function( object,... ){
                standardGeneric( "mi.matrix" )
              } 
  )
}

if ( !isGeneric( "mi.data.frame" )) {
  setGeneric( "mi.data.frame",
              function( object,... ){
                standardGeneric( "mi.data.frame" )
              } 
  )
}


if ( !isGeneric( "mi.coef" )) {
    setGeneric( "mi.coef",
               function( object )
               standardGeneric( "mi.coef" ) )
}
if ( !isGeneric( "mi.start" ) ) {
    setGeneric( "mi.start", function( object ) 
              standardGeneric( "mi.start" ) )
}
if ( !isGeneric( "mi.sigma" ) ) {
    setGeneric( "mi.sigma",
                function( object, ... )
               standardGeneric( "mi.sigma" ) )
}
if ( !isGeneric( "mi.expected" ) ) {
    setGeneric( "mi.expected",
               function( object, ... )
               standardGeneric( "mi.expected" ) )
}
if ( !isGeneric( "mi.resid" ) ) {
    setGeneric( "mi.resid",
               function( object, ... )
               standardGeneric( "mi.resid" ) )
}
if ( !isGeneric( "mi.imputed" ) ) {
    setGeneric( "mi.imputed",
               function( object, ... )
               standardGeneric( "mi.imputed" ) )
}
if ( !isGeneric("mi.hist")) {
    setGeneric( "mi.hist",
               function( Yobs, object, ... )
               standardGeneric( "mi.hist" ) )
}

#if ( !isGeneric( "mi.scatterplot" ) ) {
#    setGeneric( "mi.scatterplot",
#               function( Yobs, Yimp, ... )
#               standardGeneric( "mi.scatterplot" ) )
#}

if ( !isGeneric( "mi.plot" ) ) {
    setGeneric( "mi.plot",
               function( object, Yobs, ... )
               standardGeneric( "mi.plot" ) )
}
if ( !isGeneric( "typecast" ) ) {
    setGeneric( "typecast",
               function( object )
               standardGeneric( "typecast" ) )
}
