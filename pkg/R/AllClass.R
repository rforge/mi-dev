# ==============================================================================
# class definition 
# ==============================================================================
setClass("mi",
        representation(
            call      = "character",
            data      = "numeric",
            m         = "numeric",
            type      = "character",
            nmis      = "numeric",
            imp       = "list",
            converged = "numeric",
            bugs      = "list" ),
            contains = "list"
)
setClass("mi.info",
        representation(),
            contains = "list"
)
setClass("mi.method",
        representation(
            model = "list", 
            expected = "numeric", 
            random = "numeric"),
            contains = "list"
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
            random   = "numeric"),
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
setClass("mi.mixed",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)

setClass("mi.sqrtcontinuous",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.logcontinuous",
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
            random   = "numeric"),
            contains = "mi.method"
)
setClass("mi.copy",
        representation(
            model    = "list", 
            expected = "numeric", 
            random   = "numeric"),
            contains = "mi.method"
)
