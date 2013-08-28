\name{Wave}
\docType{methods}
\alias{Wave}
\alias{Wave,ANY-method}
\alias{Wave,WaveMC-method}
\alias{Wave,numeric-method}
\alias{Wave,matrix-method}
\alias{Wave,data.frame-method}
\alias{Wave,list-method}
\alias{[,Wave-method}
\alias{coerce,matrix,Wave-method}
\alias{coerce,data.frame,Wave-method}
\alias{coerce,list,Wave-method}
\alias{coerce,numeric,Wave-method}
\alias{coerce,Wave,data.frame-method}
\alias{coerce,Wave,matrix-method}
\alias{coerce,WaveGeneral,list-method}
\alias{coerce,Wave,WaveMC-method}
\title{Constructors and coercion for class Wave objects}
\description{Constructors and coercion for class \code{Wave} objects}
\usage{
Wave(left, ...)
\S4method{Wave}{numeric}(left, right = numeric(0), samp.rate = 44100, bit = 16, pcm = TRUE, ...)
}
\arguments{
    \item{left, right, samp.rate, bit, pcm}{See Section \dQuote{Slots} on the help page \link{Wave-class}.
        Except for numeric, the argument \code{left} can also be a matrix (1 or 2 columns), data.frame (1 or 2 columns), list (1 or 2 elements), or \code{WaveMC} (1 or 2 channels) object 
        representing the channels.}
    \item{...}{Further arguments to be passed to the numeric method.}
}                                                                                          
\value{
    An object of \link{Wave-class}.
}
\details{The class definition has been extended in \pkg{tuneR} version 1.0-0. Saved objects of class \code{Wave} generated with former versions can be 
updated with \code{\link{updateWave}} to match the new definition.}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \link{WaveMC-class}, \code{\link{writeWave}}, \code{\link{readWave}}, \code{\link{updateWave}}}
\examples{
# constructing a Wave object (1 sec.) containing sinus sound with 440Hz:
x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * sin(440 * x))
Wobj <- Wave(left = channel)
Wobj

# or more easily:
Wobj <- sine(440)
}
\keyword{methods}
\concept{Wave}
\concept{channel}
\concept{sampling}
\concept{sampling rate}
\concept{bit}
\concept{stereo}
\concept{mono}
\concept{music}
\concept{speech}
