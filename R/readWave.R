readWave <- 
function(filename){
    if(!is.character(filename))
        stop("'filename' must be of type character.")
    if(length(filename) != 1)
        stop("Please specify exactly one 'filename'.")
    if(!file.exists(filename))
        stop("File '", filename, "' does not exist.")
    if(file.access(filename, 4))
        stop("No read permission for file ", filename)

    # Open connection
    con <- file(filename, "rb")
    on.exit(close(con)) # be careful ...
    int <- integer()
    
    # Reading in the header:
    RIFF <- readChar(con, 4)
    file.length <- readBin(con, int, n = 1, size = 4)
    WAVE <- readChar(con, 4)
    if (!(RIFF == "RIFF" && WAVE == "WAVE"))
        warning("Looks like '", filename, "' is not a valid wave file.")
    FMT <- readChar(con, 4)
    # waiting for the fmt chunk
    i <- 0
    while(FMT != "fmt "){
        i <- i+1
        belength <- readBin(con, int, n = 1, size = 4)
        seek(con, where = belength, origin = "current")
        FMT <- readChar(con, 4)
        if(i > 5) stop("There seems to be no 'fmt ' chunk in this Wave (?) file.")
    }
    fmt.length <- readBin(con, int, n = 1, size = 4)
    pcm <- readBin(con, int, n = 1, size = 2)
    channels <- readBin(con, int, n = 1, size = 2)
    sample.rate <- readBin(con, int, n = 1, size = 4)
    bytes.second <- readBin(con, int, n = 1, size = 4)
    block.align <- readBin(con, int, n = 1, size = 2)
    bits <- readBin(con, int, n = 1, size = 2)
    if(fmt.length > 16)
        seek(con, where = fmt.length - 16, origin = "current")
    DATA <- readChar(con, 4)
    # waiting for the data chunk    
    i <- 0    
    while(DATA != "data"){
        i <- i+1
        belength <- readBin(con, int, n = 1, size = 4)
        seek(con, where = belength, origin = "current")
        DATA <- readChar(con, 4)
        if(i > 5) stop("There seems to be no 'data' chunk in this Wave (?) file.")
    }
    data.length <- readBin(con, int, n = 1, size = 4)
    bytes <- bits/8
    if(((sample.rate * block.align) != bytes.second) || 
        ((channels * bytes) != block.align))
            warning("Wave file '", filename, "' seems to be corrupted.")

    ## reading in sample data
    N <- data.length / bytes
    sample.data <- readBin(con, int, n = N, size = bytes, 
        signed = (bytes == 2))
    
    # Constructing the Wave object:    
    object <- new("Wave", stereo = (channels == 2), samp.rate = sample.rate, bit = bits)
    if(channels == 2) {
        sample.data <- matrix(sample.data, nrow = 2)
        object@left <- sample.data[1, ]
        object@right <- sample.data[2, ]
    }   
    else object@left <- sample.data
    
    # Return the Wave object
    return(object)
}
