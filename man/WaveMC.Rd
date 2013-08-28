\name{WaveMC}
\docType{methods}
\alias{WaveMC}
\alias{WaveMC,ANY-method}
\alias{WaveMC,Wave-method}
\alias{WaveMC,numeric-method}
\alias{WaveMC,matrix-method}
\alias{WaveMC,data.frame-method}
\alias{WaveMC,list-method}
\alias{[,WaveMC-method}
\alias{coerce,matrix,WaveMC-method}
\alias{coerce,data.frame,WaveMC-method}
\alias{coerce,list,WaveMC-method}
\alias{coerce,numeric,WaveMC-method}
\alias{coerce,WaveMC,data.frame-method}
\alias{coerce,WaveMC,matrix-method}
\alias{coerce,WaveMC,Wave-method}
\title{Constructors and coercion for class WaveMC objects}
\description{Constructors and coercion for class \code{WaveMC} objects}
\usage{
WaveMC(data, ...)
\S4method{WaveMC}{matrix}(data = matrix(numeric(0), 0, 0), samp.rate = 44100, bit = 16, pcm = TRUE, ...)
}
\arguments{
    \item{data}{Except for a numeric matrix, the argument \code{data} can also be a numeric vector (for one channel), data.frame (columns representing channels), 
        list (elements containing numeric vectors that represent the channels), or \code{Wave} object.}
    \item{samp.rate, bit, pcm}{See Section \dQuote{Slots} on the help page \link{WaveMC-class}.}
    \item{...}{Further arguments to be passed to the matrix method.}
}                                                                                          
\value{
    An object of \link{WaveMC-class}.
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\link{WaveMC-class}, \link{Wave-class}, \code{\link{writeWave}}, \code{\link{readWave}}}
\examples{
# constructing a WaveMC object (1 sec.) containing sinus sound with 440Hz:
x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * sin(440 * x))
WMCobj <- WaveMC(data = channel)
WMCobj
}
\keyword{methods}
\concept{Wave}
\concept{WaveMC}
\concept{channel}
\concept{sampling}
\concept{sampling rate}
\concept{bit}
\concept{stereo}
\concept{mono}
\concept{music}
\concept{speech}
