# ==============================================================================
# class definition 
# ==============================================================================
methods::setOldClass("lm")
methods::setOldClass("glm")

setClass("mi.info")
        


setClass("mi",
            representation(
                call      = "call",
                data      = "data.frame" ,
                m         = "numeric",
                mi.info   = "mi.info",
                imp       = "list",
                converged = "logical",
                coef.conv = "ANY",
                bugs      = "ANY"),
            contains  = "list"
)

setClass("mi.glm")

setClass("mi.lm")


setClass("mi.method",
        representation(
          model = "list",
          expected = "numeric",
          random = "numeric"),
          contains = "list"
)


setClass("mi.continuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)



setClass("mi.dichotomous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)

setClass("mi.categorical",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric",
            residuals = "numeric"),
            contains = "mi.method"
)

setClass("mi.polr",
        representation(
            model = "list", 
            expected = "numeric", 
            random = "numeric"),
            contains = "mi.method"
)

setClass("mi.continuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)

setClass("mi.fixed",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)


setClass("mi.pmm",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric",
            residuals = "numeric"),
            contains = "mi.method"
)

setClass("mi.copy",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
