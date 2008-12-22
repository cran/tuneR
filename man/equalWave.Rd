\name{equalWave}
\alias{equalWave}
\title{Checking Wave objects}
\description{Checks for some kind of equality of objects of class \code{Wave}.}
\usage{
equalWave(object1, object2)
}
\arguments{
  \item{object1, object2}{Object(s) of class \code{\link{Wave}}.}
}
\value{
    Does not return anything. 
    It \code{\link{stop}}s code execution with an error message indicating the problem 
    if the two objects don't have the same properties, i.e.
    identical sampling rate, resolution (bit), and number of channels (stereo/mono).
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\code{\link{Wave}}}
\keyword{error}
\keyword{utilities}
\concept{Wave}
