MFCC <- 
function(object, a = 0.1, HW.width = 0.025, HW.overlapping = 0.25, 
    T.number = 24, T.overlapping = NA, K = 12)
{
    .Deprecated("melfcc", package="tuneR", "'MFCC' is deprecated. Use 'melfcc' instead.")
    mc <- match.call()
    if("T.overlapping" %in% names(as.list(mc)))
        warning("T.overlapping ignored!")
    melfcc(samples=object, sr=object@samp.rate, wintime=HW.width, hoptime=(1-HW.overlapping)*HW.width,
        preemph=a, nbands=T.number, fbtype = "htkmel", numcep = K)$cepstra
    
}
