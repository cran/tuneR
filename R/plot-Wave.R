plot.Wave.channel <- 
function(x, xunit, ylim, xlab, ylab, main, nr, simplify, ...){
    null <- if(x@bit == 8) 128 else 0
    l <- length(x@left)
    at <- round((ylim[2] - null) * 2/3, -floor(log(ylim[2], 10)))
    at <- null + c(-at, 0, at)
    if(simplify && (l > nr)){
        nr <- ceiling(l / round(l / nr))
        index <- seq(1, l, length = nr)
        if(xunit == "time") index <- index / x@samp.rate
        mat <- matrix(c(x@left, rep(null, nr - (l %% nr))), 
            nrow = nr, byrow = TRUE)
        rg <- apply(mat, 1, range)
        plot(index, rg[1,], type = "n", yaxt = "n", ylim = ylim, 
            xlab = xlab, ylab = ylab, main = main, ...)
        segments(index, rg[1,], index, rg[2,], ...)
    }
    else{
        index <- seq(along = x@left)
        if(xunit == "time") index <- index / x@samp.rate
        plot(index, x@left,
            type = "l", yaxt = "n", ylim = ylim, xlab = xlab, 
            ylab = ylab, main = main, ...)
    }
    axis(2, at = at)
}
    
    
setMethod("plot", signature(x = "Wave", y = "missing"),
function(x, info = FALSE, xunit = c("time", "samples"), 
    ylim = NULL, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    simplify = TRUE, nr = 1500, ...){
    
    xunit <- match.arg(xunit)
    if(is.null(xlab)) xlab <- xunit
    stereo <- x@stereo
    l <- length(x@left)
    if(is.null(ylim)){
        ylim <- range(x@left, x@right)
        if(x@bit == 8)
            ylim <- c(-1, 1) * max(abs(ylim - 127)) + 127
        else
            ylim <- c(-1, 1) * max(abs(ylim))
    }
    if(stereo){
        opar <- par(mfrow = c(2,1), 
            oma = c(if(info) 6.1 else 5.1, 0, 4.1, 0))
        on.exit(par(opar))
        mar <- par("mar")
        par(mar = c(0, mar[2], 0, mar[4]))
        plot.Wave.channel(mono(x, "left"), xunit = xunit,
            ylab = if(is.null(ylab)) "left channel" else ylab, 
            main = NULL, sub = NULL, xlab = NULL, ylim = ylim, 
            xaxt = "n", simplify = simplify, nr = nr, ...)
        plot.Wave.channel(mono(x, "right"), xunit = xunit,
            ylab = if(is.null(ylab)) "right channel" else ylab,
            main = NULL, sub = sub, xlab = NULL, ylim = ylim,  
            simplify = simplify, nr = nr, ...)
        title(main = main, outer = TRUE, line = 2)
        title(xlab = xlab, outer = TRUE, line = 3)
        title(sub  = sub , outer = TRUE, line = 4)
        par(mar = mar)
    }
    else{
        if(info){
            opar <- par(oma = c(2, 0, 0, 0))
            on.exit(par(opar))
        }
        plot.Wave.channel(x, xunit = xunit, 
            ylab = if(is.null(ylab)) "" else ylab,
            main = main, sub = sub, xlab = xlab, ylim = ylim,
            simplify = simplify, nr = nr, ...)
    }
    if(info){
        mtext(paste("Wave Object: ",  
                l, " samples (", 
                round(l / x@samp.rate, 2),  " sec.), ",
                x@samp.rate, " Hertz, ",
                x@bit, " bit, ",
                if(stereo) "stereo." else "mono.", sep = ""), 
            side = 1, outer = TRUE, line = if(stereo) 5 else 0, ...)
    }
})
