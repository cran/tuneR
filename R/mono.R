mono <- 
function(object, which = c("left", "right")){
    if(!is(object, "Wave")) 
        stop("Object not of class 'Wave'")
    validObject(object)
    which <- match.arg(which)
    return(
        switch(which,
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
    )
}
