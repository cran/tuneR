prepComb <- function(object, zero = 0, where = c("both", "start", "end")){
    if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)
    where <- match.arg(where)
    starts <- 1
    ends <- length(object@left)    
    if(is.na(zero)) zero <- mean(object@left)
    if(!is.numeric(zero) || length(zero) != 1)
        stop("zero must be either NA or a numeric value of length 1")
    up <- which(diff(sign(object@left - zero)) > 0)
    if(length(up) > 1){
        if(where %in% c("both", "start"))
            starts <- up[1] + 1
        if(where %in% c("both", "end"))
            ends <- rev(up)[1]
    }else warning("returned object is unchanged\nat least two zero level crossings from negative to positive are required")
    return(object[starts:ends])
}
