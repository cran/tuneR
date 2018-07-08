write_4byte_unsigned_int <- function(x, con){
    if((!is.numeric(x) || is.na(x)) || (x < 0 || x > (2^32-1))) 
        stop("the field length must be a 4 byte unsigned integer in [0, 2^32-1], i.e. file size < 4 GB")
    big <- x %/% 256^2
    small <- x %% 256^2
    writeBin(as.integer(c(small, big)), con, size = 2, endian = "little")
}

write_longvector <- function(x, ..., bytes){
    if(bytes > (2^31-1)){
        index <- 1:floor(length(x)/2)
        writeBin(x[index], ...)
        writeBin(x[-index], ...)        
    }
    else writeBin(x, ...)
}



writeWave <- 
function(object, filename, extensible = TRUE) {
    if(!is(object, "WaveGeneral")) 
        stop("'object' needs to be of class 'Wave' or 'WaveMC'")
    validObject(object)
    if(is(object, "Wave")){
        object <- as(object, "WaveMC")
        colnames(object) <- c("FL", if(ncol(object) > 1) "FR")
    }
    if(ncol(object) > 2 && !extensible)
        stop("Objects with more than two columns (multi channel) can only be written to a Wave extensible format file.")

    cn <- colnames(object)
    if((length(cn) != ncol(object) || !all(cn %in% MCnames[["name"]])) || any(duplicated(cn)))
        stop("colnames(object) must be specified and must uniquely identify the channel ordering for WaveMC objects, see ?MCnames for possible channels")
    cnamesnum <- as.numeric(factor(colnames(object), levels=MCnames[["name"]]))
    if(is.unsorted(cnamesnum))
        object <- object[,order(cnamesnum)]
    dwChannelMask <- sum(2^(cnamesnum - 1))  ##

    l <- as.numeric(length(object)) # can be an int > 2^31
    sample.data <- t(object@.Data)
    dim(sample.data) <- NULL
    
    ## PCM or IEEE FLOAT
    pcm <- object@pcm                                 
    
    if(pcm) {                                                                                     
      if((object@bit == 8) && ( (max(sample.data) > 255) || (min(sample.data) < 0) ))              
          stop("for 8-bit Wave files, data range is supposed to be in [0, 255], see ?normalize")
      if((object@bit == 16) && ( (max(sample.data) > 32767) || (min(sample.data) < -32768)))
          stop("for 16-bit Wave files, data range is supposed to be in [-32768, 32767], see ?normalize")
      if((object@bit == 24) && ( (max(sample.data) > 8388607) || (min(sample.data) < -8388608)))
          stop("for 24-bit Wave files, data range is supposed to be in [-8388608, 8388607], see ?normalize")
      if((object@bit == 32) && ( (max(sample.data) > 2147483647) || (min(sample.data) < -2147483648)))
          stop("for 32-bit Wave files, data range is supposed to be in [-2147483648, 2147483647], see ?normalize")
      if(any(sample.data %% 1 > 0))
          warning("channels' data will be rounded to integers for writing the wave file")
    } else {                                                                                    
      if( (max(sample.data) > 1) || (min(sample.data) < -1) )
          stop("for IEEE float Wave files, data range is supposed to be in [-1,1], see ?normalize") 
    }
    
    # Open connection
    con <- file(filename, "wb")
    on.exit(close(con)) # be careful ...
        
    # Some calculations:
    byte <- as.integer(object@bit / 8)
    channels <- ncol(object)
    block.align <- channels * byte
    bytes <- l * byte * channels

    if((!is.numeric(bytes) || is.na(bytes)) || (bytes < 0 || (round(bytes + if(extensible) 72 else 36) + 4) > (2^32-1)))
        stop(paste("File size in bytes is", round(bytes + if(extensible) 72 else 36) + 4, "but must be a 4 byte unsigned integer in [0, 2^32-1], i.e. file size < 4 GB"))

    ## Writing the header:
    # RIFF
    writeChar("RIFF", con, 4, eos = NULL) 
    write_4byte_unsigned_int(round(bytes + if(extensible) 72 else 36), con) # cksize RIFF
    
    
    # WAVE
    writeChar("WAVE", con, 4, eos = NULL)
    # fmt chunk
    writeChar("fmt ", con, 4, eos = NULL)
    if(extensible) { # cksize format chunk
        writeBin(as.integer(40), con, size = 4, endian = "little") 
    } else {
        writeBin(as.integer(16), con, size = 4, endian = "little")
    }    
    if(!extensible) { # wFormatTag
        writeBin(as.integer(if(pcm) 1 else 3), con, size = 2, endian = "little")
    } else {
        writeBin(as.integer(65534), con, size = 2, endian = "little") # wFormatTag: extensible   
    }
    writeBin(as.integer(channels), con, size = 2, endian = "little") # nChannels
    writeBin(as.integer(object@samp.rate), con, size = 4, endian = "little") # nSamplesPerSec
    writeBin(as.integer(object@samp.rate * block.align), con, size = 4, endian = "little") # nAvgBytesPerSec
    writeBin(as.integer(block.align), con, size = 2, endian = "little") # nBlockAlign
    writeBin(as.integer(object@bit), con, size = 2, endian = "little") # wBitsPerSample
    # extensible
    if(extensible) {
        writeBin(as.integer(22), con, size = 2, endian = "little") # cbsize extensible
        writeBin(as.integer(object@bit), con, size = 2, endian = "little") # ValidBitsPerSample
        writeBin(as.integer(dwChannelMask), con, size = 4, endian = "little") #  dbChannelMask
        writeBin(as.integer(if(pcm) 1 else 3), con, size = 2, endian = "little") # SubFormat 1-2
        writeBin(as.raw(c(0,   0,   0,  0,  16,   0, 128,   0 ,  0, 170,   0,  56, 155, 113)), con) # SubFormat 3-16
        # fact
        writeChar("fact", con, 4, eos = NULL)
        writeBin(as.integer(4), con, size = 4, endian = "little") # cksize fact chunk
        writeBin(as.integer(l), con, size = 4, endian = "little") # dwSampleLength
    }
    # data
    writeChar("data", con, 4, eos = NULL)
    write_4byte_unsigned_int(round(bytes), con)

    # Write data:
    # PCM format
    if(pcm) { 
      if(byte == 3){
          sample.data <- sample.data + 2^24 * (sample.data < 0)
          temp <- sample.data %% (256^2)
          sample.data <- sample.data %/% 256^2
          a2 <- temp %/% 256
          temp <- temp %%  256
          write_longvector(as.integer(rbind(temp, a2, sample.data)), con, size = 1, endian = "little", bytes=bytes)
      } else {
          write_longvector(as.integer(sample.data), con, size = byte, endian = "little", bytes=bytes)
      }
    } else {
      write_longvector(as.numeric(sample.data), con, size = byte, endian = "little", bytes=bytes)
    }

    invisible(NULL)
}
