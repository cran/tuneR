setMethod("plot", signature(x = "Wspec", y = "missing"),
function(x, which = 1, type = "h", xlab = "frequency", ylab = NULL, log = "", ...){

    if(is.null(ylab)){
        ylab <- if(x@normalize) "normalized periodogram" else "periodogram"
        if(!missing(log) && (log == "y")) ylab <- paste("log(", ylab, ")", sep = "")
    }        
    spec <- x@spec[[which]]
    plot(x@freq, spec, type = type, 
        xlab = xlab, ylab = ylab, log = log, ...)
})


setMethod("plot", signature(x = "WspecMat", y = "missing"),
function(x, xlab = "time", ylab = "frequency", xunit = c("samples", "time"), log = "", ...){
    if(log == "z"){ 
        x@spec <- log(x@spec)
        log <- ""
    }
    xunit <- match.arg(xunit)
    if(xunit == "time"){
        x@starts <- x@starts / x@samp.rate
    }
    image(x@starts, x@freq, x@spec, xlab = xlab, ylab = ylab, log = log, ...)
    
    # actually image plots centered at x location, hence for low resolution the offset shown is incorrect and we'd rather need
    #    offset <- diff(x@starts[1:2])/2
    #    image(x@starts + offset, x@freq, x@spec, xlab = xlab, ylab = ylab, log = log, ...)
    # but this produces an ugly x axis with 0 excluded from the tick marks ...
})

setMethod("image", signature(x = "Wspec"),
function(x, xlab = "time", ylab = "frequency", xunit = c("samples", "time"), log = "", ...){
    x <- as(x, "WspecMat")
    xunit <- match.arg(xunit)    
    plot(x, xlab = xlab, ylab = ylab, xunit = xunit, log = log, ...)
})
