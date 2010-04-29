## decode.R - decode MP3 files
##
## Author: Olaf Mersmann (OME) <olafm@statistik.tu-dortmund.de>

readMP3 <- function(filename){
    data <- readBin(filename, raw(), n = file.info(filename)$size)
    .Call("do_read_mp3", data, package = "tuneR")
}
