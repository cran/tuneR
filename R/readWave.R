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
    FMT <- readChar(con, 4)
    fmt.length <- readBin(con, int, n = 1, size = 4)
    pcm <- readBin(con, int, n = 1, size = 2)
    channels <- readBin(con, int, n = 1, size = 2)
    sample.rate <- readBin(con, int, n = 1, size = 4)
    bytes.second <- readBin(con, int, n = 1, size = 4)
    block.align <- readBin(con, int, n = 1, size = 2)
    bits <- readBin(con, int, n = 1, size = 2)
    DATA <- readChar(con, 4)
    data.length <- readBin(con, int, n = 1, size = 4)

    bytes <- bits / 8
    
    # Checking header infos for validity:
    if(!(RIFF == "RIFF" && WAVE == "WAVE" && FMT == "fmt " && 
        (DATA == "data" || DATA == "PAD ") && fmt.length == 16 && pcm == 1))
            warning("Looks like '", filename, "' is not a valid wave file.")
    if(((sample.rate * block.align) != bytes.second) || 
        ((channels * bytes) != block.align))
            warning("Wave file '", filename, "' seems to be corrupted.")

    # Wild guess:
    if(DATA == "PAD "){
        N <- data.length / bytes
        data.length <- file.length - 40 - data.length
        pad <- readBin(con, int, n = N, size = bytes, signed = (bytes == 2))
        if(any(pad)) stop("This is not a valid wave file.")
        DATA <- readChar(con, 4)
        if(DATA != "data")
            warning("Looks like '", filename, "' is not a valid wave file.")
    }
        

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
