\name{panorama}
\alias{panorama}
\title{Narrow the Panorama of a Stereo Sample}
\description{Narrow the panorama of a stereo Wave object.}
\usage{panorama(object, pan = 1)}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{pan}{Value in [-1,1] to narrow the panorama, see the Details below. 
    The default (1) does not change anything.}
}
\details{
If \code{abs(pan) < 1}, mixtures of the two channels of the \code{Wave} objects 
are used for the left and the right channel 
of the returned Sample object, so that they appear closer to the center. 

For \code{pan = 0}, both sounds are completely in the center (i.e. averaged).

If \code{pan < 0}, the left and the right channel are interchanged.
}
\value{Wave object with the transformed panorama.}
\author{Uwe Ligges, based on code by Matthias Heymann}
\seealso{\code{\link{Wave}}}
\keyword{manip}
\concept{Wave}
\concept{channel}
\concept{stereo}
