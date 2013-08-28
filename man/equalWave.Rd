\name{equalWave}
\alias{equalWave}
\title{Checking Wave objects}
\description{Internal S4 generic function that checks for some kind of equality of objects of class \code{Wave} or class \code{WaveMC}.}
\usage{
equalWave(object1, object2)
}
\arguments{
  \item{object1, object2}{Object(s) of class \code{\link{Wave}} or class \code{\link{WaveMC}} (both of the same class).}
}
\value{
    Does not return anything. 
    It \code{\link{stop}}s code execution with an error message indicating the problem 
    if the objects are not of the same class (either \code{Wave} oder \code{WaveMC}) or if 
    the two objects don't have the same properties, i.e.
    identical sampling rate, resolution (bit), and number of channels (for \code{WaveMC}, resp. stereo/mono for \code{Wave}).
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}}
\keyword{error}
\keyword{utilities}
\concept{Wave}
