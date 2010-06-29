equalWave <- 
function(object1, object2){
    if(!(is(object1, "Wave") && is(object2, "Wave"))) 
        stop("Object not of class 'Wave'")
    if(!(validObject(object1) && validObject(object2)))
        stop("Not a valid 'Wave' object")
    if(object1@samp.rate != object2@samp.rate)
        stop("Sampling Rate of 'Wave' objects differ")    
    if(object1@bit != object2@bit)
        stop("Bit resolution of 'Wave' objects differ")    
    if(xor(object1@stereo, object2@stereo))
        stop("One 'Wave' object is mono, the other one stereo")
}
