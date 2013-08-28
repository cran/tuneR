\name{normalize-methods}
\docType{methods}
\alias{normalize}
\alias{normalize,Wave-method}
\alias{normalize,WaveMC-method}

\title{Rescale the range of values}
\description{Centering and rescaling the waveform of a \code{Wave} or \code{WaveMC} object to a canonical interval 
corresponding to the Wave format (e.g. [-1, 1], [0, 254], 
    [-32767, 32767], [-8388607, 8388607], or [-2147483647, 2147483647]).}
\usage{
normalize(object, unit = c("1", "8", "16", "24", "32", "64", "0"), 
    center = TRUE, level = 1, rescale = TRUE, pcm = object@pcm)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}} or \code{\link{WaveMC}}.}
    \item{unit}{Unit to rescale to.\cr
        \code{"1"} (default) for rescaling to numeric values in [-1, 1],\cr
        \code{"8"} (i.e. 8-bit) for rescaling to integers in [0, 254],\cr
        \code{"16"} (i.e. 16-bit) for rescaling to integers in [-32767, 32767],\cr
        \code{"24"} (i.e. 24-bit) for rescaling to integers in [-8388607, 8388607],\cr
        \code{"32"} (i.e. 32-bit) for rescaling either to integers in [-2147483647, 2147483647] 
                    (PCM Wave format if \code{pcm=TRUE}) or to numeric values in [-1, 1] 
                    (FLOAT_IEEE Wave format if \code{pcm = FALSE}),\cr
        \code{"64"} (i.e. 64-bit) for rescaling to real values in [-1, 1] (FLOAT_IEEE Wave format), and\cr
        \code{"0"} for not rescaling (hence only centering if \code{center = TRUE}).}
  \item{center}{If \code{TRUE} (default), values are centered around 0 (or 127 if \code{unit = "8"}).}
  \item{level}{Maximal percentage of the amplitude used for normalizing (default is 1).} 
  \item{rescale}{Logical, whether to rescale to the maximal possible dynamic range.}
  \item{pcm}{Logical. By default, the \code{pcm} information from the \code{object} is kept.
    Otherwise, if \code{TRUE}, the object is coerced to the PCM Wave format. If \code{FALSE}, the object is coerced to the FLOAT_IEEE format, i.e. numeric values in [-1, 1].}
}
\value{
  An object containing the normalized data of the same class as the input \code{object}, 
    i.e. either \code{\link{Wave}} or \code{\link{WaveMC}}.
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg,
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\code{\link{writeWave}}, \link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}}
\keyword{manip}
\concept{Wave}

