\name{noSilence}
\docType{methods}
\alias{noSilence}
\alias{noSilence,Wave-method}
\alias{noSilence,WaveMC-method}
\title{Cut off silence from a Wave or WaveMC object}
\description{Generic function to cut off silence or low noise at the beginning and/or at the end of an object of class \code{Wave} or class \code{WaveMC}.}
\usage{
noSilence(object, zero = 0, level = 0, where = c("both", "start", "end"))
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.}
  \item{zero}{The zero level (default: 0) at which ideal cut points are determined (see Details).
    A typical alternative would be 127 for 8 bit \code{\link{Wave}} or \code{\link{WaveMC}} objects.
    If \code{zero = NA}, the mean of the left Wave channel (for \code{Wave}, resp. the mean of the first channel for \code{WaveMC}) is taken as zero level.}
  \item{level}{Values in the interval between \code{zero} and \code{zero - level}/\code{zero + level}
    are considered as silence.}  
  \item{where}{One of \code{"both"} (default), \code{"start"}, or \code{"end"}
    indicating at \code{where} to prepare the \code{\link{Wave}} or \code{\link{WaveMC}} object for concatenation.}
}
\value{
    An object of class \code{\link{Wave}} or \code{\link{WaveMC}}.
}
\details{Silcence is removed at the locations given by \code{where} of the \code{\link{Wave}} or \code{\link{WaveMC}} object,
    where silence is defined such that (in both channels if stereo, in all channels if multichannel for \code{WaveMC}) all values are in 
    the interval between \code{zero - level} and \code{zero + level}.
    All values before (or after, respectively) the first non-silent value are removed from the object.
    }
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg,
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\code{\link{silence}}, \link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}, \code{\link{extractWave}}}
\keyword{manip}
\keyword{utilities}
\concept{silence}
\concept{cut}
\concept{noise}
\concept{Wave}
\concept{WaveMC}
