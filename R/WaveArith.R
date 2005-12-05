setMethod("Arith", signature(e1 = "Wave", e2 = "Wave"),
function(e1, e2){
    equalWave(e1, e2)    
    if(length(e1@left) != length(e2@left))
        stop("Waves must be of equal length for Arithmetics")
    e1@left <- callGeneric(e1@left, e2@left)
    if(e1@stereo)
        e1@right <- callGeneric(e1@right, e2@right)
    e1
})

setMethod("Arith", signature(e1 = "numeric", e2 = "Wave"),
function(e1, e2){
    validObject(e2)
    if(!is.element(length(e1), c(1, length(e2@left))))
        stop("Waves must be of equal length for Arithmetics")
    e2@left <- callGeneric(e1, e2@left)
    if(e2@stereo)
        e2@right <- callGeneric(e1, e2@right)
    e2
})

setMethod("Arith", signature(e1 = "Wave", e2 = "numeric"),
function(e1, e2){
    validObject(e1)
    if(!is.element(length(e2), c(1, length(e1@left))))
        stop("Waves must be of equal length for Arithmetics")
    e1@left <- callGeneric(e1@left, e2)
    if(e1@stereo)
        e1@right <- callGeneric(e1@right, e2)
    e1
})

setMethod("Arith", signature(e1 = "Wave", e2 = "missing"),
function(e1, e2){
    validObject(e1)
    e1@left <- callGeneric(e1@left)
    if(e1@stereo)
        e1@right <- callGeneric(e1@right)
    e1
})
