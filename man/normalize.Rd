\name{normalize}
\alias{normalize}
\title{Rescale the range of values}
\description{Centering and rescaling the waveform of a Wave object to either [-1,1], [0, 254], or [-32767, 32767].}
\usage{
normalize(object, unit = c("1", "8", "16", "0"), center = TRUE, level = 1)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}}.}
    \item{unit}{Unit to rescale to. 
        \code{"1"} (default) for rescaling to real values in [-1,1], 
        \code{"8"} (i.e. 8-bit) for rescaling to integers in [0, 254],
        \code{"16"} (i.e. 16-bit) for rescaling to integers in [-32767, 32767], and
        \code{"0"} for not rescaling (hence only centering, if \code{center=TRUE}).}
  \item{center}{If \code{TRUE} (default), values are centered around 0 (or 127, if \code{unit="8"}).}
  \item{level}{Maximal percentage of the amplitude used for normalizing (default is 1).} 
}
\value{
  An object of class \code{\link{Wave}}.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de},
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\code{\link{Wave}}, \code{\link{writeWave}}}
\keyword{manip}
\concept{Wave}

