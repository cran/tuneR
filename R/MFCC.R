## stolen and generalized (added "ce" stuff) from e1071:
hamming.window <- function (n) 
{
    if (n == 1) 
        c <- 1
    else {
        n <- n - 1
        c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
    }
    return(c)
}

stftMFCC <- 
function (X, win = min(80, floor(length(X)/10)), inc = min(24, floor(length(X)/30)), 
    coef = 64, wtype = hamming.window)
{
    numcoef <- 2 * coef
    if (win > numcoef) {
        win <- numcoef
        warning("stft: window size adjusted to", win, ".\n")
    }
    numwin <- trunc((length(X) - win)/inc)
    wincoef <- wtype(win)  # symmetrical around (win-1)/2 + 1, floor(win) weights
    z <- matrix(0, numwin + 1, numcoef)
    y <- z
    st <- 1
    ce <- numeric(numwin)                                      
    for (i in 0:numwin) {
        z[i + 1, 1:win] <- X[st:(st + win - 1)] * wincoef  # X[floor(st)] gets first weight etc.
        y[i + 1, ] <- fft(z[i + 1, ])
        ce[i + 1] <- floor(st) + (win - 1)/2                   
        st <- st + inc
    }
    Y <- list(values = Mod(y[, 1:coef]), windowsize = win, increment = inc, 
        windowtype = wtype, center = ce)
    class(Y) <- "stft"
    return(Y)
}


## MFCC
MFCC <- 
function(object, a = 0.1, HW.width = 0.025, HW.overlapping = 0.25, 
    T.number = 24, T.overlapping = 0.5, K = 12)
{
    if(!is(object, "Wave")) 
        stop("'object' needs to be of class 'Wave'")
    validObject(object)

    if(object@stereo) stop("Stereo processing not yet implemented...")

    S <- object@left
    sampling.rate <- object@samp.rate
    S.length <- length(S)
    
    # frequency based amplification:
    S.fil <- filter(S, filter = c(1, -a))     # S.fil(n) = S(n) - a*S(n-1) is filtered signal
    S.fil <- S.fil[-S.length]                 # remove NAs, which arise out of filter process
    
    # divide into frames und Discrete Fourier Transformation (DFT):
    HW.length <- sampling.rate * HW.width         # number of samples in HW.width, width of hamming windows
    HW.shift <- HW.length * (1 - HW.overlapping)  # shift of windows
    coefficients <- floor(S.length/2) + 1         # number of Fourier-coefficients
    STDFT <- stftMFCC(S.fil, win = HW.length, inc = HW.shift, 
                      coef = coefficients, wtype = hamming.window)
    HW.number <- dim(STDFT$values)[1]             # number of hamming windows
    time <- (STDFT$center - 1)/(sampling.rate - 1)# time, which belongs to window-centers
    frequency <- (0:(coefficients - 1))/(2*coefficients) * sampling.rate # conversion of Fourier-coefficients to frequencies (in Hz)
    
    # computations of mel-frequency-cepstral-coefficients:
    Mel <- 2595 * log10(1 + frequency/700)             # conversion of frequencies from Hz to mel
    T.length <- (max(Mel) - min(Mel))/(T.number * (1 - T.overlapping) + T.overlapping)          # length of triangle windows
    T.centers <- min(Mel) + .5 * T.length + (0:(T.number - 1)) * (1 - T.overlapping) * T.length # centers of triangular windows
    T.windows <- matrix(0, coefficients, T.number)     # T.windows is matrix of dimension coefficients * T.number = 24
    for(m in 1:T.number){
        T.windows[,m] <- 1 - (abs(Mel - T.centers[m]))/(.5 * T.length)
        T.windows[T.windows[,m] < 0, m] <- 0           # T.windows contains the weights of the T.number windows, 
    }                                                  # which belong to the frequencies, in coloumns 
    IDCT <- sapply(1:K, function(x) cos(x * (1:T.number - .5) * pi/T.number) ) # IDCT is matrix of dimension T.number * K
    X <- log(abs(STDFT$values %*% T.windows)) %*% IDCT # X is the result of the IDCT of Y.log
    X <- cbind(time, X)
    return(X)
}
#obj <- readWave("d:/fceg0sx78_aadr8_1.wav")
#a2 <- MFCC(obj, a = 0.1, HW.width = 0.025, HW.overlapping = 0.25, T.number = 24, T.overlapping = 0.5, K = 15)
