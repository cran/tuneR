\name{Wave}
\docType{methods}
\alias{Wave}
\alias{Wave-methods}
\alias{Wave,ANY-method}
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
\alias{coerce,Wave,list-method}
\title{Constructors and coercion for class Wave objects}
\description{Constructors and coercion for class Wave objects}
\usage{
Wave(left, ...)
\S4method{Wave}{numeric}(left, right = numeric(0), samp.rate = 44100, bit = 16, ...)
}
\arguments{
    \item{left, right, samp.rate, bit}{See Section \dQuote{Slots}.}
    \item{...}{Further arguments to be passed to the default method \code{Wave.default}.}
}
\value{
    An object of \link{Wave-class}.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{writeWave}}, \code{\link{readWave}}}
\examples{
# constructing a Wave object (1 sec.) containing sinus sound with 440Hz:
x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * sin(440 * x))
Wobj <- Wave(left = channel)
Wobj

# or more easily:
Wobj <- sine(440, bit = 16)
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
