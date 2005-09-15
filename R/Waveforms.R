preWaveform <- function(freq, duration, from, xunit, samp.rate){
    if (!is.numeric(duration) || duration <= 0 || length(duration) != 1)
        stop("'duration' must be a positive numeric of length 1")
    if (!is.numeric(from) || from < 0 || length(from) != 1)
        stop("'from' must be a positive numeric of length 1")
    if (!is.numeric(samp.rate) || samp.rate < 0 || length(samp.rate) != 1)
        stop("'samp.rate' must be a positive numeric of length 1")
    if(!is.numeric(freq) || freq <= 0 || length(freq) != 1)
        stop("'freq' must be a positive numeric of length 1")
    if(xunit == "time"){
        duration <- duration * samp.rate
        from <- from * samp.rate
    }
    return(c(duration = round(duration), from = round(from)))
}

postWaveform <- function(channel, samp.rate, bit, stereo, ...){
    if(!is.numeric(bit) || length(bit)!=1 || (!bit %in% c(0,1,8,16)))
        stop("'bit' must be an integer of length 1 in in {0,1,8,16}")
    if(bit == 8)
        channel <- channel + 127
    if(stereo && !is.matrix(channel))
        channel <- matrix(channel, ncol = 2, nrow = length(channel))
    Wobj <- Wave(channel, samp.rate = samp.rate, 
                 bit = if(bit %in% 0:1) 16 else bit, ...)
    normalize(Wobj, unit = as.character(bit), center = FALSE)
}

silence <- function(duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
                    stereo = FALSE, xunit = c("samples", "time"), ...){
    xunit <- match.arg(xunit)
    durFrom <- preWaveform(freq = 1, duration = duration, from = from, 
        xunit = xunit, samp.rate = samp.rate)
    channel <- rep(0, durFrom["duration"])
    postWaveform(channel = channel, samp.rate = samp.rate, 
        bit = bit, stereo = stereo, ...)
}

sine <- function(freq, duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
                 stereo = FALSE, xunit = c("samples", "time"), ...){
    xunit <- match.arg(xunit)
    durFrom <- preWaveform(freq = freq, duration = duration, from = from, 
        xunit = xunit, samp.rate = samp.rate)
    channel <- sin(2 * pi * freq * (durFrom["from"]:sum(durFrom)) / samp.rate)
    postWaveform(channel = channel, samp.rate = samp.rate, 
        bit = bit, stereo = stereo, ...)
}

sawtooth <- function(freq, duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
                 stereo = FALSE, xunit = c("samples", "time"), reverse = FALSE, ...){
    xunit <- match.arg(xunit)
    durFrom <- preWaveform(freq = freq, duration = duration, from = from, 
        xunit = xunit, samp.rate = samp.rate)
    channel <- seq(durFrom["from"], 2*freq*sum(durFrom), 
        length = durFrom["duration"]) %% 2 - 1
    if(!is.logical(reverse) || length(reverse) != 1)
        stop("'reverse' must be a logical value of length 1")
    if(reverse) channel <- rev(channel)
    postWaveform(channel = channel, samp.rate = samp.rate, 
        bit = bit, stereo = stereo, ...)
}

square <- function(freq, duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
                 stereo = FALSE, xunit = c("samples", "time"), up = 0.5, ...){
    xunit <- match.arg(xunit)
    durFrom <- preWaveform(freq = freq, duration = duration, from = from, 
        xunit = xunit, samp.rate = samp.rate)
    if(!is.numeric(up) || length(up) != 1 || max(abs(up)) > .5)
        stop("'up' must be a numeric in [-0.5, 0.5] of length 1")
    channel <- sign(seq(durFrom["from"], freq*sum(durFrom), 
                        length = durFrom["duration"])
                    %% 1 - 1 + up)
    postWaveform(channel = channel, samp.rate = samp.rate, 
        bit = bit, stereo = stereo, ...)
}

noise <- function(kind = c("white", "pink"), duration = samp.rate,
                  samp.rate = 44100, bit = 1, stereo = FALSE, 
                  xunit = c("samples", "time"), ...){
    xunit <- match.arg(xunit)
    kind <- match.arg(kind)
    durFrom <- preWaveform(freq = 1, duration = duration, from = 0, 
        xunit = xunit, samp.rate = samp.rate)
    N <- durFrom["duration"] * (stereo + 1)
    ru <- runif(N, min = -1, max = 1)
    channel <- 
        switch(kind,
            white = ru,
            pink = {
                f <- 1 / sqrt(seq(length.out = N))
                f <- f * N / (2 * sum(f))
                Re(fft(fft(ru) * f, inverse = TRUE))
            }
        )
    channel <- matrix(channel, ncol = (stereo + 1))
    postWaveform(channel = channel, samp.rate = samp.rate, 
        bit = bit, stereo = stereo, ...)
}
