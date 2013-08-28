\name{Wave-class}
\docType{class}
\alias{Wave-class}
\title{Class Wave}
\description{Class \dQuote{Wave}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Wave", ...)},
or more conveniently using the function \code{\link{Wave}}.
}
\section{Slots}{
  \describe{
    \item{\code{left}:}{Object of class \code{"numeric"} representing the left channel.}
    \item{\code{right}:}{Object of class \code{"numeric"} representing the right channel,
        \code{NULL} if mono.}
    \item{\code{stereo}:}{Object of class \code{"logical"} indicating whether this
        is a stereo (two channels) or mono representation.}
    \item{\code{samp.rate}:}{Object of class \code{"numeric"} - the sampling rate, e.g. 44100 for CD quality.}
    \item{\code{bit}:}{Object of class \code{"numeric"}, common is 16 for CD quality, or
        8 for a rather rough representation.}
    \item{\code{pcm}:}{Object of class \code{"logical"} indicating whether this is a 
        PCM or IEEE_FLOAT Wave format.}
  }
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\details{The class definition has been extended in \pkg{tuneR} version 1.0-0. Saved objects of class Wave generated with former versions can be 
updated with \code{\link{updateWave}} to match the new definition.
}
\seealso{\code{\link{Wave}}, \code{\link{updateWave}}, and for multi channel Wave files see \link{WaveMC-class}}
\keyword{classes}
\concept{Wave}
\concept{channel}
\concept{sampling}
\concept{sampling rate}
\concept{bit}
\concept{stereo}
\concept{mono}
\concept{music}
\concept{speech}
