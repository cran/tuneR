normalize <- function(object, unit = c("1", "8", "16", "24", "32", "0"), center = TRUE, level = 1){
    if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)
    unit <- match.arg(unit)
    if(!(unit %in% c("1", "8", "16", "24", "32", "0")))
        stop("'unit' must be either 1 (real valued norm.), 8 (norm. to 8-bit), 16 (norm. to 16-bit), 24 (...), or 32 (...)")
    if(center){
        object@left <- object@left - mean(object@left)
        object@right <- object@right - mean(object@right)    
    }
    if(object@bit == 8 && all(object@right >= 0) && all(object@left >= 0)){
        object@left <- object@left - 127
        object@right <- object@right - 127    
    }   
    if(unit != "0"){
        m <- max(abs(c(range(object@left), if(object@stereo) range(object@right))))
        object@left <- level * object@left / m
        object@right <- level * object@right / m
        if(unit == "8"){
            object@left <- round(object@left * 127 + 127)
            object@right <- round(object@right * 127 + 127)
        }
        else if(unit == "16"){
            object@left <- round(object@left * 32767)
            object@right <- round(object@right * 32767)
        }    
        else if(unit == "24"){
            object@left <- round(object@left * 8388607)
            object@right <- round(object@right * 8388607)
        }    
        else if(unit == "32"){
            object@left <- round(object@left * 2147483647)
            object@right <- round(object@right * 2147483647)
        }    
    }
    return(object)
}
