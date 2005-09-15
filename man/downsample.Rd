\name{downsample}
\alias{downsample}
\title{Downsampling a Wave object}
\description{Downsampling an object of class \code{Wave}.}
\usage{
downsample(object, samp.rate)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}}.}
    \item{samp.rate}{Sampling rate the object is to be downsampled to.
        \code{samp.rate} must be in \code{[2000, 192000]}; 
        typical values are 11025, 22050, and 44100 for CD quality.
        If the \code{object}'s sampling rate is already equal or smaller than \code{samp.rate},
        the \code{object} will be returned unchanged.}
}
\value{
    An object of class \code{\link{Wave}}.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{Wave}}}
\keyword{manip}
\keyword{utilities}
\concept{Wave}
\concept{sampling}
\concept{sampling rate}
\concept{CD quality}
