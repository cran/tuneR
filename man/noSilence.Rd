\name{noSilence}
\alias{noSilence}
\title{Cut off silence from a Wave object}
\description{Cut off silence or low noise at the beginning and/or at the end of an object of class \code{Wave}.}
\usage{
noSilence(object, zero = 0, level = 0, where = c("both", "start", "end"))
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{zero}{The zero level (default: 0) at which ideal cut points are determined (see Details).
    A typical alternative would be 127 for 8 bit \code{\link{Wave}} objects.
    If \code{zero=NA}, the mean of the left Wave channel is taken as zero level.}
  \item{level}{Values in the interval between \code{zero} and \code{zero - level}/\code{zero + level}
    are considered as silence.}  
  \item{where}{One of \dQuote{both} (default), \dQuote{start}, or \dQuote{end}
    indicating at \code{where} to prepare the \code{\link{Wave}} object for concatenation.}
}
\value{
    An object of class \code{\link{Wave}}.
}
\details{Silcence is removed at the locations given by \code{where} of the \code{\link{Wave}} object,
    where silence is defined such that (in both channels, if stereo) all values are in 
    the interval between \code{zero} and \code{zero - level}/\code{zero + level}.
    All values before (or after, respectively) the first non-silent value are removed from the object.
    }
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}, 
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\code{\link{silence}}, \link{Wave-class}, \code{\link{Wave}}, \code{\link{extractWave}}}
\keyword{manip}
\keyword{utilities}
\concept{silence}
\concept{cut}
\concept{noise}
\concept{Wave}
