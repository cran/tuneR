bind <- 
function(...){
    allobjects <- as.list(list(...))
    object <- allobjects[[1]]
    lapply(allobjects[-1], equalWave, object)
    object@left <- unlist(lapply(allobjects, slot, "left"))
    if(object@stereo)
        object@right <- unlist(lapply(allobjects, slot, "right"))
    return(object)
}
