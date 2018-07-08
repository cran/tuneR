readVarLength <- function(con){
  b <- numeric(4)
  b[1] <- readBin(con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
  bytes <- 1
  if(b[1] > 127){
    b[1] <- (b[1]-128) * 2^7
    b[2] <- readBin(con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
    bytes <- 2
    if(b[2] > 127){
        b[2] <- b[2]-128
        b <- b*2^7
        b[3] <- readBin(con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
        bytes <- 3
        if(b[3] > 127){
            b[3] <- b[3]-128
            b <- b*2^7
            b[4] <- readBin(con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
            bytes <- 4
        }
    }
  }
  c(sum(b), bytes)
}

readMTrkEvent <- function(con, lastEventChannel = NA){
    DTtemp <- readVarLength(con)
    DT <- DTtemp[1]
    EventChannel <- readBin(con, raw(0), n = 1)
    event <- substr(EventChannel, 1, 1)
    backseeked <- 0
    if(event < "8"){
        seek(con, where = -1, origin = "current")
        EventChannel <- lastEventChannel
        event <- substr(EventChannel, 1, 1)
        backseeked <- 1
    }
    eventName <- switch(event,
        "8" = "Note Off",
        "9" = "Note On",
        "a" = "Note Aftertouch",
        "b" = "Controller",
        "c" = "Program Change",
        "d" = "Channel Aftertouch",
        "e" = "Pitch Bend",
        "f" = "Meta or System")
    if(EventChannel == "ff"){
        type <- as.character(readBin(con, raw(0), n = 1))
# FIXME: We need a special case for each possible meta event. A data.frame is not the best possible solution..., 
# probably define separate S4 classes?
        elength <- readVarLength(con)
        eventName <- switch(type,
            "00" = "Sequence Number",
            "01" = "Text Event",
            "02" = "Copyright Notice",
            "03" = "Sequence/Track Name",
            "04" = "Instrument Name",
            "05" = "Lyric",
            "06" = "Marker",
            "07" = "Cue Point",
            "08" = "Program Name",
            "09" = "Device Name",            
            "20" = "MIDI Channel Prefix",
            "21" = "MIDI Port",
            "2f" = "End of Track",
            "51" = "Set Tempo",
            "54" = "SMPTE Offset",
            "58" = "Time Signature",
            "59" = "Key Signature",
            "7f" = "Sequencer Specific",
                   "Meta")

        if(type > "00" && type < "10")
            eventData <- readChar(con, elength[1])
        else
            eventData <- paste(as.character(switch(type,
                "00" = readBin(con, integer(0), n = 1, size = elength[1], endian = "big", signed = FALSE),
                "20" = readBin(con, integer(0), n = 1, size = elength[1], endian = "big", signed = FALSE),
                "21" = readBin(con, integer(0), n = 1, size = elength[1], endian = "big", signed = FALSE),
                "51" = as.vector(readBin(con, integer(0), n = 3, size = 1, endian = "big", signed = FALSE) %*% c(256^2, 256, 1)),
                "58" = {temp <- readBin(con, integer(0), n = 4, size = 1, endian = "big", signed = FALSE) 
                        paste0(temp[1], "/", 2^temp[2], ", ", temp[3], " clocks/tick, ", temp[4], " 1/32 notes / 24 clocks")
                       },
                "59" = {sharpflat <- readBin(con, integer(0), n = 1, size = 1, endian = "big")
                        majorminor <- readBin(con, integer(0), n = 1, size = 1, endian = "big")
                        ma <- paste(c("B (H)", "Gb", "Db", "Ab", "Eb", "Bb", "F", "C", "G", "D", "A", "E", "B (H)", "F#", "C#"), "major")
                        mi <- paste(c("Ab","Eb", "Bb", "F",  "C",  "G",  "D", "A", "E", "B (H)", "F#","C#","G#","D#", "A#"), "minor")
                        if(majorminor == 1) mi[sharpflat+8] else ma[sharpflat+8]
                       },
                       readBin(con, raw(0), n = elength[1])
            )), collapse=" x ")

        return(list(deltatime=DT, event=eventName, type=type, channel = NA, parameter1=NA, parameter2=NA, 
                    parameterMetaSystem=eventData, bytes=2+sum(elength)+DTtemp[2]-backseeked, EventChannel=EventChannel))
    }
    if(event == "f"){
        eventName <- "System"
        elength <- readVarLength(con)
        seek(con, where = elength[1], origin = "current")
        return(list(deltatime=DT, event=eventName, type=NA, channel = NA, parameter1=NA, parameter2=NA, 
                    parameterMetaSystem=NA, bytes=1+sum(elength)+DTtemp[2]-backseeked, EventChannel=EventChannel))
    }
    channel <- as.numeric(rawShift(EventChannel, 4)) / 2^4
    parameter1 <- readBin(con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")
    parameter2 <- if(event %in% c("c", "d")) NA else readBin(con, integer(0), n = 1, size = 1, signed = FALSE, endian = "big")    
    return(list(deltatime=DT, event=eventName, type=NA, channel = channel, parameter1=parameter1, parameter2=parameter2, 
                parameterMetaSystem=NA, bytes=2+DTtemp[2]+ !(event %in% c("c", "d"))-backseeked, EventChannel=EventChannel))
}

readMidi <- function(file){
  con <- file(description = file, open = "rb")
  on.exit(close(con))
  MThd <- readChar(con, 4)
  if(MThd != "MThd") 
    stop("No Header Chunk in this Midi (?)")
  MThd_length <- readBin(con, integer(0), n = 1, size = 4, endian = "big")
  if(MThd_length != 6) 
    stop("Unexpected Header Chunk size") 
  MThd_format <- readBin(con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE)
  if(!(MThd_format %in% 0:2)) 
    stop("Unexpected Mide file format") 
  MThd_tracks <- readBin(con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE)

# FIXME: MThd_division < 0 can appear in Midi files with a very different interpretation!
  MThd_division <- readBin(con, integer(0), n = 1, size = 2, signed = TRUE, endian = "big")
  if(MThd_division < 0){
        stop("Midi representation of timing: Frames per second / ticks per frame not yet implemented, please ask the author")
  }
  
  allTracks <- list()
  for(track in 1:MThd_tracks){
      MTrk <- readChar(con, 4)
      if(MTrk != "MTrk") 
        stop("No Track Chunk in this Midi")
      MTrk_length <- readBin(con, integer(0), n = 1, size = 4, endian = "big")
      MeventList <- list()
      bytes <- 0
      i <- 0
      while(bytes < MTrk_length){
        i <- i+1
        MeventList[[i]] <- readMTrkEvent(con, if(i>1)  MeventList[[i-1]][["EventChannel"]] else NA)
        bytes <- bytes + MeventList[[i]][["bytes"]]
      }
      if( MeventList[[i]][["type"]] != "2f") 
        stop("No end of track event after track length bytes")
      thisTrack <- do.call("rbind", lapply(MeventList, function(x) as.data.frame(x, stringsAsFactors=FALSE)))
      thisTrack <- thisTrack[,-(ncol(thisTrack)-(0:1))] # remove bytes and EventChannel information
      thisTrack[,1] <- cumsum(thisTrack[,1])
      names(thisTrack)[1] <- "time"
      thisTrack$track <- track
      allTracks[[track]] <- thisTrack
    }  
    allTracks <- do.call("rbind", allTracks)
    allTracks$event[allTracks$event == "Note On" & allTracks$parameter2 == 0] <- "Note Off"
    allTracks$event <- factor(allTracks$event, 
        levels = c("Note Off", "Note On", "Note Aftertouch", "Controller", "Program Change", "Channel Aftertouch", 
            "Pitch Bend", "Meta", "System", "Sequence Number", "Text Event", "Copyright Notice", 
            "Sequence/Track Name", "Instrument Name", "Lyric", "Marker", "Cue Point", "Program Name", 
            "Device Name", "MIDI Channel Prefix", "MIDI Port",
            "End of Track", "Set Tempo", "SMPTE Offset", "Time Signature", "Key Signature", "Sequencer Specific"))
    allTracks
}
