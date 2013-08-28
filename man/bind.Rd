\name{bind}
\docType{methods}
\alias{bind}
\alias{bind,Wave-method}
\alias{bind,WaveMC-method}
\title{Concatenating Wave objects}
\description{Generic function for concatenating objects of class \code{Wave} or \code{WaveMC}.}
\usage{
bind(object, ...)
\S4method{bind}{Wave}(object, ...)
\S4method{bind}{WaveMC}(object, ...)
}
\arguments{
  \item{object, \dots}{Objects of class \code{\link{Wave}} or class \code{\link{WaveMC}}, each of the same class and of the same kind (checked by \code{\link{equalWave}}),
   i.e. identical sampling rate, resolution (bit), and number of channels (for \code{WaveMC}, resp. stereo/mono for \code{Wave}).}
}
\value{
    An object of class \code{\link{Wave}} or class \code{\link{WaveMC}} that corresponds to the class of the input.
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\code{\link{prepComb}} for preparing the concatenation, \link{Wave-class},
         \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}, \code{\link{extractWave}}, \code{\link{stereo}}}
\keyword{manip}
\keyword{utilities}
\concept{Wave}

