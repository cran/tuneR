\name{normalize}
\alias{normalize}
\title{Rescale the range of values}
\description{Centering and rescaling the waveform of a Wave object to either [-1,1], [0, 254], 
    [-32767, 32767], [-8388607, 8388607], or [-2147483647, 2147483647].}
\usage{
normalize(object, unit = c("1", "8", "16", "24", "32", "0"), 
    center = TRUE, level = 1)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}}.}
    \item{unit}{Unit to rescale to.\cr
        \code{"1"} (default) for rescaling to real values in [-1,1],\cr
        \code{"8"} (i.e. 8-bit) for rescaling to integers in [0, 254],\cr
        \code{"16"} (i.e. 16-bit) for rescaling to integers in [-32767, 32767],\cr
        \code{24} (i.e. 24-bit) for rescaling to integers in [-8388607, 8388607],\cr
        \code{32} (i.e. 32-bit) for rescaling to integers in [-2147483647, 2147483647], and\cr
        \code{"0"} for not rescaling (hence only centering, if \code{center=TRUE}).}
  \item{center}{If \code{TRUE} (default), values are centered around 0 (or 127, if \code{unit="8"}).}
  \item{level}{Maximal percentage of the amplitude used for normalizing (default is 1).} 
}
\value{
  An object of class \code{\link{Wave}}.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de},
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \code{\link{writeWave}}}
\keyword{manip}
\concept{Wave}

