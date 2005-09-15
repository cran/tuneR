require(methods)
##########
# define class Wave
setClass("Wave",
    representation = representation(left = "numeric",
    right = "numeric", stereo = "logical",
    samp.rate = "numeric", bit = "numeric"),
    prototype = prototype(stereo = TRUE, samp.rate = 44100, 
        bit = 16))

setValidity("Wave", 
function(object){
    if(!is(object@left, "numeric")) return("channels of Wave objects bust be numeric")
    if(!(is(object@stereo, "logical") && (length(object@stereo) < 2)))
        return("slot 'stereo' of a Wave object must be a logical of length 1")
    if(object@stereo){
        if(!is(object@right, "numeric"))
            return("channels of Wave objects bust be numeric")
        if(length(object@left) != length(object@right))
            return("both channels of Wave objects must have the same length")
    }
    else if(length(object@right))
        return("'right' channel of a wave object is not supposed to contain data if slot stereo==FALSE")
    if(!(is(object@samp.rate, "numeric") &&
        (length(object@samp.rate) < 2) && (object@samp.rate > 0)))
            return("slot 'samp.rate' of a Wave object must be a positive numeric of length 1")
    if(!(is(object@bit, "numeric") &&
        (length(object@bit) < 2) && (object@bit %in% c(8, 16))))
            return("slot 'bit' of a Wave object must be a positive numeric (either 8 or 16) of length 1")
    return(TRUE)
})

setMethod("[", signature(x = "Wave"),
function(x, i, j, ..., drop=FALSE){
    if(!is(x, "Wave")) 
        stop("'x' needs to be of class 'Wave'")
    validObject(x)
    x@left <- x@left[i]
    if(x@stereo)
        x@right <- x@right[i]
    return(x)
})

##########
# Wave object generating functions
setGeneric("Wave",
function(left, ...) standardGeneric("Wave"))

setMethod("Wave", signature(left = "numeric"), 
function(left, right = numeric(0), samp.rate = 44100, bit = 16, ...){
    if(missing(samp.rate)) 
        warning("'samp.rate' not specified, assuming 44100Hz")
    if(missing(bit)) 
        warning("'bit' not specified, assuming 16bit")
    return(
        new("Wave", stereo = length(right) > 0, samp.rate = samp.rate, 
            bit = bit, left = left, right = right))
})

setMethod("Wave", signature(left = "matrix"), 
function(left, ...)
    Wave(as.data.frame(left), ...)
)

setMethod("Wave", signature(left = "data.frame"), 
function(left, ...)
    Wave(as.list(left), ...)
)

setMethod("Wave", signature(left = "list"), 
function(left, ...){
    if(length(left) > 1){
        if(all(c("left", "right") %in% names(left)))
            Wave(left$left, left$right, ...)
        else 
            Wave(left[[1]], left[[2]], ...)
    }
    else Wave(left[[1]], ...)
})


setAs("matrix", "Wave", function(from, to) Wave(from))
setAs("data.frame", "Wave", function(from, to) Wave(from))
setAs("list", "Wave", function(from, to) Wave(from))
setAs("numeric", "Wave", function(from, to) Wave(from))
setAs("Wave", "data.frame", 
function(from, to){
    dat <- if(from@stereo) data.frame(left = from@left, right = from@right) 
           else data.frame(mono = from@left)
    return(dat)
})
setAs("Wave", "matrix", function(from, to) 
    return(as(as(from, "data.frame"), "matrix")))
setAs("Wave", "list", function(from, to)
    return(as(as(from, "data.frame"), "list")))


setMethod("show", signature(object = "Wave"), 
function(object){
    l <- length(object@left)
    cat("\nWave Object")
    cat("\n\tNumber of Samples:     ", l)
    cat("\n\tDuration (seconds):    ",
        round(l / object@samp.rate, 2))
    cat("\n\tSamplingrate (Hertz):  ", object@samp.rate)
    cat("\n\tChannels (Mono/Stereo):",
        if(object@stereo) "Stereo" else "Mono")
    cat("\n\tBit (8/16):            ", object@bit, "\n\n")
})

setMethod("summary", signature(object = "Wave"), 
function(object, ...){
    l <- length(object@left)
    cat("\nWave Object")
    cat("\n\tNumber of Samples:     ", l)
    cat("\n\tDuration (seconds):    ",
        round(l / object@samp.rate, 2))
    cat("\n\tSamplingrate (Hertz):  ", object@samp.rate)
    cat("\n\tChannels (Mono/Stereo):",
        if(object@stereo) "Stereo" else "Mono")
    cat("\n\tBit (8/16):            ", object@bit)
    cat("\n\nSummary statistics for channel(s):\n\n")
    if(object@stereo)
        print(rbind(left = summary(object@left), right = summary(object@right)))
    else print(summary(object@left))
    cat("\n\n")
})
