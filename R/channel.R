channel <- 
function(object, which = c("both", "left", "right")){
    if(!is(object, "Wave")) 
        stop("Object not of class 'Wave'")
    validObject(object)
    which <- match.arg(which)
    return(as.data.frame(
        switch(which,
            both = object,
            left = {
                object@stereo <- FALSE
                object@right <- numeric(0)
                object
            },
            right = {
                object@left <- object@right
                object@stereo <- FALSE
                object@right <- numeric(0)
                object
            }
        )
    ))
}
