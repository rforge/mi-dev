# ==============================================================================
# extract coefficient values
# ==============================================================================

setMethod("coef", signature(object = "mi.method"), 
  function(object){
  object@model$coefficients
}
)


setMethod("coefficients", signature(object = "mi.method"), 
  function(object){
  object@model$coefficients
}
)
