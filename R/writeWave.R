writeWave <- 
function(object, filename){
    if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)

    if(object@stereo){
        sample.data <- matrix(c(object@left, object@right), nrow = 2, byrow = TRUE)
        dim(sample.data) <- NULL
    }
    else sample.data <- object@left

    if((object@bit == 8) && ( (max(sample.data) > 255) || (min(sample.data) < 0) ))
        stop("for 8-bit Wave files, data range is supposed to be in [0, 255], see ?normalize")
    if((object@bit == 16) && ( (max(sample.data) > 32767) || (min(sample.data) < -32768)))
        stop("for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize")
    if((object@bit == 24) && ( (max(sample.data) > 8388607) || (min(sample.data) < -8388608)))
        stop("for 24-bit Wave files, data range is supposed to be in [-8388608, 8388607], see ?normalize")
    if((object@bit == 32) && ( (max(sample.data) > 2147483647) || (min(sample.data) < -2147483648)))
        stop("for 32-bit Wave files, data range is supposed to be in [-2147483648, 2147483647], see ?normalize")
    if(any(sample.data %% 1)) 
        warning("channels' data will be rounded to integers for writing the wave file")

    # Open connection
    con <- file(filename, "wb")
    on.exit(close(con)) # be careful ...
        
    # Some calculations:
    l <- length(object@left)
    byte <- as.integer(object@bit / 8)
    channels <- object@stereo + 1
    block.align <- channels * byte
    bytes <- l * byte * channels
        
    # Writing the header:
    writeChar("RIFF", con, 4, eos = NULL)
    writeBin(as.integer(bytes + 36), con, size = 4, endian = "little")
    writeChar("WAVE", con, 4, eos = NULL)
    writeChar("fmt ", con, 4, eos = NULL)
    writeBin(as.integer(16), con, size = 4, endian = "little")    
    writeBin(as.integer(1), con, size = 2, endian = "little")
    writeBin(as.integer(channels), con, size = 2, endian = "little")
    writeBin(as.integer(object@samp.rate), con, size = 4, endian = "little")
    writeBin(as.integer(object@samp.rate * block.align), con, size = 4, endian = "little")
    writeBin(as.integer(block.align), con, size = 2, endian = "little")
    writeBin(as.integer(object@bit), con, size = 2, endian = "little")
    writeChar("data", con, 4, eos = NULL)
    writeBin(as.integer(bytes), con, size = 4, endian = "little")

    # Write data: 
    writeBin(as.integer(sample.data), con, size = byte, endian = "little")
    invisible(NULL)
}
