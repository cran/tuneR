stereo <- 
function(left, right){
    ln <- deparse(substitute(left))
    rn <- deparse(substitute(right))
    equalWave(left, right)
    if(right@stereo)
        stop(rn, " already is a stereo 'Wave' object")
    if(left@stereo)
        stop(ln, " already is a stereo 'Wave' object")
    if(length(right@left) != length(left@left))
        stop("Channel length of ", ln, " and ", rn, " differ")
    left@stereo <- TRUE
    left@right <- right@left
    return(left)
}
