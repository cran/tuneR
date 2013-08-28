\name{downsample}
\alias{downsample}
\title{Downsampling a Wave or WaveMC object}
\description{Downsampling an object of class \code{Wave} or class \code{WaveMC}.}
\usage{
downsample(object, samp.rate)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.}
    \item{samp.rate}{Sampling rate the object is to be downsampled to.
        \code{samp.rate} must be in \code{[2000, 192000]}; 
        typical values are 11025, 22050, and 44100 for CD quality.
        If the \code{object}'s sampling rate is already equal or smaller than \code{samp.rate},
        the \code{object} will be returned unchanged.}
}
\value{
    An object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}}
\keyword{manip}
\keyword{utilities}
\concept{Wave}
\concept{WaveMC}
\concept{sampling}
\concept{sampling rate}
\concept{CD quality}
