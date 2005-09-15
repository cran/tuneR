setGeneric("play",
function(object, player, ...) standardGeneric("play"))

setMethod("play", signature(object = "character", player = "ANY"),
function(object, player, ...){
    if(missing(player)){
        player <- getWavPlayer()
        if(.Platform$OS.type == "windows" && is.null(player)){
            player <- "mplay32"
            if(missing(...))
                player <- paste(player, "/play /close")
        }
    }
    system(paste(player, ..., object))
})

setMethod("play", signature(object = "Wave", player = "ANY"),
function(object, player, ...){
    filename <- "tuneRtemp.wav"
    wd <- getwd()
    setwd(tempdir())
    on.exit({unlink(filename); setwd(wd)})
    writeWave(object, filename)
    play(filename, player, ...)
})
