\name{panorama}
\docType{methods}
\alias{panorama}
\alias{panorama,Wave-method}
\alias{panorama,WaveMC-method}
\title{Narrow the Panorama of a Stereo Sample}
\description{Generic function to narrow the panorama of a stereo \code{Wave} or \code{WaveMC} object.}
\usage{panorama(object, pan = 1)}
\arguments{
  \item{object}{Object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.}
  \item{pan}{Value in [-1,1] to narrow the panorama, see the Details below. 
    The default (1) does not change anything.}
}
\details{
If \code{abs(pan) < 1}, mixtures of the two channels of the \code{Wave} or \code{WaveMC} objects 
are used for the left and the right channel of the returned Sample object if the object is of class \code{Wave}, resp. 
for the first and second channel of the returned Sample object if the object is of class \code{WaveMC}, 
so that they appear closer to the center. 

For \code{pan = 0}, both sounds are completely in the center (i.e. averaged).

If \code{pan < 0}, the left and the right channel (for \code{Wave} objects, the first and the second channel for \code{WaveMC} objects) are interchanged.
}
\value{An object of class \code{\link{Wave}} or class \code{\link{WaveMC}} with the transformed panorama.}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg, based on code by Matthias Heymann}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}}
\keyword{manip}
\concept{Wave}
\concept{WaveMC}
\concept{channel}
\concept{stereo}
