readWave <- 
function(filename, from = 1, to = Inf, 
    units = c("samples", "seconds", "minutes", "hours"), header = FALSE){
        
    if(!is.character(filename))
        stop("'filename' must be of type character.")
    if(length(filename) != 1)
        stop("Please specify exactly one 'filename'.")
    if(!file.exists(filename))
        stop("File '", filename, "' does not exist.")
    if(file.access(filename, 4))
        stop("No read permission for file ", filename)

    ## Open connection
    con <- file(filename, "rb")
    on.exit(close(con)) # be careful ...
    int <- integer()
    
    ## Reading in the header:
    RIFF <- readChar(con, 4)
    file.length <- readBin(con, int, n = 1, size = 4, endian = "little")
    WAVE <- readChar(con, 4)
    if (!(RIFF == "RIFF" && WAVE == "WAVE"))
        warning("Looks like '", filename, "' is not a valid wave file.")
    FMT <- readChar(con, 4)
    ## waiting for the fmt chunk
    i <- 0
    while(FMT != "fmt "){
        i <- i+1
        belength <- readBin(con, int, n = 1, size = 4, endian = "little")
        seek(con, where = belength, origin = "current")
        FMT <- readChar(con, 4)
        if(i > 5) stop("There seems to be no 'fmt ' chunk in this Wave (?) file.")
    }
    fmt.length <- readBin(con, int, n = 1, size = 4, endian = "little")
    pcm <- readBin(con, int, n = 1, size = 2, endian = "little")
    if(!(pcm %in% c(0, 1)))
        stop("Only PCM/uncompressed Wave formats supported")
    channels <- readBin(con, int, n = 1, size = 2, endian = "little")
    sample.rate <- readBin(con, int, n = 1, size = 4, endian = "little")
    bytes.second <- readBin(con, int, n = 1, size = 4, endian = "little")
    block.align <- readBin(con, int, n = 1, size = 2, endian = "little")
    bits <- readBin(con, int, n = 1, size = 2, endian = "little")
    if(fmt.length > 16)
        seek(con, where = fmt.length - 16, origin = "current")
    DATA <- readChar(con, 4)
    ## waiting for the data chunk    
    i <- 0    
    while(DATA != "data"){
        i <- i+1
        belength <- readBin(con, int, n = 1, size = 4, endian = "little")
        seek(con, where = belength, origin = "current")
        DATA <- readChar(con, 4)
        if(i > 5) stop("There seems to be no 'data' chunk in this Wave (?) file.")
    }
    data.length <- readBin(con, int, n = 1, size = 4, endian = "little")
    bytes <- bits/8
    if(((sample.rate * block.align) != bytes.second) || 
        ((channels * bytes) != block.align))
            warning("Wave file '", filename, "' seems to be corrupted.")

    ## If only header required: construct and return it
    if(header){
        return(list(sample.rate = sample.rate, channels = channels, 
            bits = bits, samples = data.length / (channels * bytes)))
    }

    ## convert times to sample numbers
    fctr <- switch(match.arg(units),
                   samples = 1,
                   seconds = sample.rate,
                   minutes = sample.rate * 60,
                   hours = sample.rate * 3600)
    if(fctr > 1) {
        from <- round(from * fctr + 1)
        to <- round(to * fctr)
    } 

    ## calculating from/to for reading in sample data    
    N <- data.length / bytes
    N <- min(N, to*channels) - (from*channels+1-channels) + 1
    seek(con, where = (from - 1) * bytes * channels, origin = "current")

    ## reading in sample data   
    sample.data <- readBin(con, int, n = N, size = bytes, 
        signed = (bytes == 2), endian = "little")
    
    ## Constructing the Wave object:    
    object <- new("Wave", stereo = (channels == 2), samp.rate = sample.rate, bit = bits)
    if(channels == 2) {
        sample.data <- matrix(sample.data, nrow = 2)
        object@left <- sample.data[1, ]
        object@right <- sample.data[2, ]
    }   
    else object@left <- sample.data

    ## Return the Wave object
    return(object)
}
