\name{show-WaveWspec-methods}
\docType{methods}
\alias{show,Wave-method}
\alias{show,WaveMC-method}
\alias{show,Wspec-method}
\alias{show,WspecMat-method}
\title{Showing objects}
\description{Showing \code{Wave}, \code{Wspec}, and \code{WspecMat} objects.}
\section{Methods}{
\describe{
\item{object = "Wave"}{The \code{Wave} object is being \code{show}n.
    The number of samples, duration in seconds, Samplingrate (Hertz),
    Stereo / Mono, PCM / IEEE, and the resolution in bits are printed.
    Note that it does not make sense to print the whole channels 
    containing several thousands or millions of samples.}
\item{object = "WaveMC"}{The \code{WaveMC} object is being \code{show}n.
    The number of samples, duration in seconds, Samplingrate (Hertz),
    number of channels, PCM / IEEE, and the resolution in bits are printed.
    Note that it does not make sense to print the whole channels 
    containing several thousands or millions of samples.}
\item{object = "Wspec"}{The number of periodograms, Fourier frequencies, window width (used amount of data), 
    amount of overlap of neighboring windows, and whether the periodogram(s) has/have been normalized
    will be printed.}
\item{object = "WspecMat"}{The number of periodograms, Fourier frequencies, window width (used amount of data), 
    amount of overlap of neighboring windows, and whether the periodogram(s) has/have been normalized
    will be printed.}
}}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}, \code{\link{Wspec}}, \code{\link{WspecMat}}, 
    \code{\link{plot-methods}}, \code{\link{summary-methods}},
    and \code{\link{periodogram}} for the constructor function and some examples}
\keyword{methods}
\keyword{print}
\concept{Wave}
\concept{periodogram}
