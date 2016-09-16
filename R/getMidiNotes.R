getMidiNotes <- function(x, ...){
    x <- x[x$event %in% c("Note On", "Note Off"), c("time", "event", "channel", "parameter1", "parameter2", "track")]
    if(!nrow(x)){
        return(data.frame(time=numeric(0), length=numeric(0), track=integer(0), channel=numeric(0), note=integer(0), 
                          notename=factor(NULL, levels = notenames(-69:62, ...)), velocity=integer(0)))
    }
    x <- split(x, x$parameter1)
    x <- lapply(x, function(i){
        Time <- matrix(i$time, byrow=TRUE, ncol=2)
        i <- i[i$event == "Note On",]
        i$length <- Time[,2]-Time[,1]
        i
    })
    x <- do.call("rbind", x)
    x$parameter1 <- as.integer(x$parameter1)
    x$notename <- factor(notenames(x$parameter1 - 69, ...), levels = notenames(-69:62, ...))
    x <- x[order(x$track, x$time), !(names(x) %in% "event")]
    names(x) <- c("time", "channel", "note", "velocity", "track", "length", "notename")
    rownames(x) <- NULL
    x <- x[,c("time", "length", "track", "channel", "note", "notename", "velocity")]
    x
}
