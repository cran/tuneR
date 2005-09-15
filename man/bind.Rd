\name{bind}
\alias{bind}
\title{Concatenating Wave objects}
\description{Concatenating objects of class \code{Wave}.}
\usage{
bind(...)
}
\arguments{
  \item{\dots}{Objects of class \code{\link{Wave}}, each of the same kind (checked by \code{\link{equalWave}}),
   i.e. identical  sampling rate, resolution (bit), and number of channels (stereo/mono).}
}
\value{
    An object of class \code{\link{Wave}}.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{prepComb}} for preparing the concatenation, 
         \code{\link{Wave}}, \code{\link{extractWave}}, \code{\link{stereo}}}
\keyword{manip}
\keyword{utilities}
\concept{Wave}

