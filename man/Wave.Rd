\name{Wave-class}
\docType{class}
\docType{methods}
\alias{Wave}
\alias{Wave-methods}
\alias{Wave,ANY-method}
\alias{Wave,numeric-method}
\alias{Wave,matrix-method}
\alias{Wave,data.frame-method}
\alias{Wave,list-method}
\alias{Wave-class}
\alias{[,Wave-method}
\alias{coerce,matrix,Wave-method}
\alias{coerce,data.frame,Wave-method}
\alias{coerce,list,Wave-method}
\alias{coerce,numeric,Wave-method}
\alias{coerce,Wave,data.frame-method}
\alias{coerce,Wave,matrix-method}
\alias{coerce,Wave,list-method}
\title{Class Wave and constructors}
\description{Class \dQuote{Wave} and its constructor functions}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Wave", ...)},
or more conveniently using the function \code{Wave}.
}
\usage{
Wave(left, ...)
\S4method{Wave}{numeric}(left, right = numeric(0), samp.rate = 44100, bit = 16, ...)
}
\arguments{
    \item{left, right, samp.rate, bit}{See Section \dQuote{Slots}.}
    \item{...}{Further arguments to be passed to the default method \code{Wave.default}.}
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
  }
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{writeWave}}, \code{\link{readWave}}}
\examples{
# constructing a Wave object (1 sec.) containing sinus sound with 440Hz:
x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * sin(440 * x))
Wobj <- Wave(left = channel)
Wobj
}
\keyword{classes}
\keyword{methods}
