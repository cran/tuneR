periodogram <-
function(object, width = length(object@left), overlap = 0,
    starts = NULL, ends = NULL, taper = 0, normalize = TRUE, ...)
{
   if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)

    if(object@stereo) stop("Stereo processing not yet implemented...")
    
    testwidth <- 2^ceiling(log(width, 2))
    if(width !=  testwidth) {
        width <- testwidth
        warning("'width' must be a potence of 2, hence using the ceiling: ", width, "\n")
    }
        
    Wspec <- new("Wspec")
    Wspec@stereo <- object@stereo
    Wspec@samp.rate <- object@samp.rate
    Wspec@taper <- taper
    temp <- width / 2
    Wspec@freq <- object@samp.rate * seq(1, width / 2) / width

    wo <- width - overlap
    lo <- length(object@left)
    lw <- lo - width
    n <- lw %/% wo
    add <- lw - n*wo
    lo <- lo + add
    dat <- c(object@left, rep(0, add))
    dat <- dat - mean(dat)
    if(normalize) 
        dat <- dat / max(abs(dat))
        
    n <- n + 1
    if(is.null(starts) && is.null(ends)){
        starts <- seq(1, lo-width+1, by = wo)
        ends <- seq(width, lo, by = wo)
    }
    else{
        if(is.null(starts)) starts <- ends - width
        if(is.null(ends)) ends <- starts + width
    }

    temp <- spec.pgram(dat[starts[1]:ends[1]], taper = taper,
        pad = 0, fast = TRUE, demean = FALSE, detrend = FALSE,
        plot = FALSE, na.action = na.fail, ...)

    Wspec@kernel <- temp$kernel
    Wspec@df <- temp$df
    Wspec@starts <- starts
    Wspec@width <- width
    Wspec@overlap <- overlap
    Wspec@normalize <- normalize
    
    spec <- vector(n, mode = "list")
    spec[[1]] <- temp$spec
    for(i in (seq(along = starts[-1]) + 1)){
        spec[[i]] <- spec.pgram(dat[starts[i]:ends[i]], taper = taper, 
            pad = 0, fast = TRUE, demean = FALSE, detrend = FALSE,
            plot = FALSE, na.action = na.fail, ...)$spec
        
    }
    Wspec@spec <- if(normalize)
        lapply(spec, function(x){
            sx <- sum(x)
            lx <- length(x)
            if(!sx) rep(1 / lx, lx) else x / sum(x)
        })
        else spec

    Wspec@variance <- mapply(function(x,y) var(dat[x:y]), starts, ends)
    Wspec@energy <- 20 * log10(mapply(function(x,y) sum(abs(dat[x:y])), starts, ends))
    
    return(Wspec)
}
