setMethod("plot", signature(x = "Wspec", y = "missing"),
function(x, which = 1, type = "h", xlab = "Frequency", ylab = NULL, log = "", ...){

    if(is.null(ylab)){
        ylab <- if(x@normalize) "normalized periodogram" else "periodogram"
        if(!missing(log) && (log == "y")) ylab <- paste("log(", ylab, ")", sep = "")
    }        
    spec <- x@spec[[which]]
    plot(x@freq, spec, type = type, 
        xlab = xlab, ylab = ylab, log = log, ...)
})
