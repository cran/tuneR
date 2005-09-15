panorama <- function(object, pan = 1){
    if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)
    if(!is.numeric(pan) || abs(pan) > 1)
    stop("'pan' must be numeric in [-1, 1].")
    if(!object@stereo){
        warning("panorama called on a mono Wave object, returned object is unchanged")
        return(object)
    }
    pan <- pan/2
    right <- object@right
    object@right <- (0.5 + pan) * right + (0.5 - pan) * object@left
    object@left  <- (0.5 - pan) * right + (0.5 + pan) * object@left
    return(object)
}
