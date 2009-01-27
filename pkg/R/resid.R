# ==============================================================================
# extract residual values for mi.method class object
# ==============================================================================

setMethod("residuals", signature(object = "mi.method"), 
  function(object, y){
  y - fitted(object)
}
)

setMethod("residuals", signature(object = "mi.dichotomous"), 
  function(object, y){
  .dichot(y) - fitted(object)
}
)

setMethod("residuals", signature(object = "mi.categorical"), 
  function(object){
  object@residuals
}
)


setMethod("resid", signature(object = "mi.method"), 
  function(object, y){
  y - fitted(object)
}
)

setMethod("resid", signature(object = "mi.dichotomous"), 
  function(object, y){
  .dichot(y) - fitted(object)
}
)

setMethod("resid", signature(object = "mi.categorical"), 
  function(object){
  object@residuals
}
)
