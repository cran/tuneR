melodyplot <- 
function(object, observed, expected = NULL, bars = NULL, main = NULL, 
    xlab = NULL, ylab = "note", xlim = NULL, ylim = NULL, 
    observedcol = "red", expectedcol = "grey", gridcol = "grey",
    lwd = 2, las = 1, cex.axis = 0.9, mar = c(5, 4, 4, 4) + 0.1){

    par(las = las, cex.axis = cex.axis, mar = mar)
  
    notenames <- c(rep(" ", 7), "C", "C#", "D", "D#", "E", "F", 
        "F#", "G", "G#", "A", "A#", "B", "c", "c#", "d", "d#", 
        "e", "f", "f#", "g", "g#", "a", "a#", "b", "c'", "c#'", 
        "d'", "d#'", "e'", "f'", "f#'", "g'", "g#'", "a'", "a#'", 
        "b'", "c''", "c#''", "d''", "d#''", "e''", "f''", "f#''", 
        "g''", "g#''", "a''", "a#''", "b''", "c'''", "c#'''", 
        "d'''", "d#'''", "e'''", "f'''", "f#'''", "g'''", "g#'''", 
        "a'''", "a#'''", "b'''", rep(" ", 14))
    notenames <- data.frame(notenum = -40:40, notename = I(notenames))

    if(is.null(bars)){
        starts <- object@starts
        bars <- starts[length(starts)] + object@width
        bars <- bars / object@samp.rate 
        if(is.null(xlab)) xlab <- "time"
    }
    else if(is.null(xlab)) xlab <- "bar"
    
    observed[observed == -100] <- NA
    rg <- range(observed, expected, na.rm = TRUE)
    y.ticks <- c("silence", 
        notenames$notename[which(notenames$notenum %in% rg[1]:rg[2])])
    rg.s <- rg[1] - 2
    observed[is.na(observed)] <- rg.s
    x <- bars * seq(0, 1, length = length(observed))
    if(is.null(xlim)) xlim <- c(0, if(bars < 1) 1 else bars)
    if(is.null(ylim)) ylim <- c(rg.s - 2, rg[2] + 0.5)
    plot(x, observed - .05, xaxt = "n", yaxt = "n", 
        main = main, xlab = xlab, ylab = ylab, 
        type = "n", lwd = lwd, xaxs = "i", yaxs = "i",
        xlim = xlim, ylim = ylim)
    axis(1, at = 1:bars)
    if(!is.null(expected)) 
        rect(c(0, x[-length(x)]), expected - 0.5, x, expected + 0.5, 
            col = expectedcol, border = expectedcol)    
    axis(2, at = (rg.s:rg[2])[-2], label = as.character(y.ticks))
    lines(x, observed + .05, col = observedcol, lwd = 2)
    abline(v = 1:(bars), col = gridcol)
    abline(h = rg[1]:(rg[2]-1) + 0.5, col = gridcol)
    abline(h = rg.s + 1.5)
    energy <- object@energy
    energy <- rg.s - 2 + 
        (3.5 * (energy - min(energy)) / diff(range(energy)))
    lines(x, energy)
    mtext("energy", side = 4, line = 2.5, at = rg.s - 0.25, las = 3)
    axis(4, at = rg.s + c(-2, 1.5), 
        label = round(range(object@energy), 1))
    box()   
}
