\name{WaveMC-class}
\docType{class}
\alias{WaveMC-class}
\title{Class WaveMC}
\description{Class \dQuote{WaveMC}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("WaveMC", ...)},
or more conveniently using the function \code{\link{WaveMC}}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"} containing numeric data, where each column is representing one channel.
        Column names are the appropriate way to name different channels. 
        The data object \code{\link{MCnames}} contains a data frame of standard names for channels in multi channel Wave files.}
    \item{\code{samp.rate}:}{Object of class \code{"numeric"} - the sampling rate, e.g. 44100 for CD quality.}
    \item{\code{bit}:}{Object of class \code{"numeric"}, common is 16 for CD quality, or
        8 for a rather rough representation.}
    \item{\code{pcm}:}{Object of class \code{"logical"} indicating whether this is a 
        PCM or IEEE_FLOAT Wave format.}
  }
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\details{This class has been added in \pkg{tuneR} version 1.0-0 for representation and construction 
of multi channel Wave files. Objects of class \code{Wave} can be transformed to the new class definition 
by calls of the form \code{as(..., "WaveMC")}. Coercion from the \code{WaveMC} class to the \link{Wave-class} 
works via  \code{as(..., "Wave")} if there are no more than 2 channels.
Coercing back to the \link{Wave-class} can be useful since some (very few) functions cannot yet deal with multi channel Wave objects.

Note that also the \link{Wave-class} definition has been extended 
in \pkg{tuneR} version 1.0-0. For more details see \link{Wave-class}.}
\seealso{\code{\link{WaveMC}}, \link{Wave-class}, \code{\link{MCnames}}}
\keyword{classes}
\concept{WaveMC}
\concept{channel}
\concept{sampling}
\concept{sampling rate}
\concept{bit}
\concept{stereo}
\concept{mono}
\concept{music}
\concept{speech}
