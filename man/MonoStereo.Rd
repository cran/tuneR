\name{Mono-Stereo}
\alias{mono}
\alias{stereo}
\title{Converting (extracting, joining) stereo to mono and vice versa}
\description{Functions to extract a channel from a stereo \code{Wave} object,
    and to join channels of two monophonic \code{Wave} objects to a stereophonic one.}
\usage{
mono(object, which = c("left", "right"))
stereo(left, right)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}}.}
    \item{which}{Character, indicating whether the \dQuote{left} or \dQuote{right} channel should be extracted.}
    \item{left}{Object of class \code{\link{Wave}} containing \code{mono}phonic sound,
        to be used for the left channel.}
    \item{right}{Object of class \code{\link{Wave}} containing \code{mono}phonic sound,
        to be used for the right channel.}    
}
\value{
    An object of class \code{\link{Wave}}.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{Wave}}}
\examples{
x <- seq(0, 2*pi, length = 44100)
Wobj <- Wave(left = round(32000 * sin(440 * x)))
Wobj
Wobj2 <- stereo(Wobj, Wobj)
Wobj2
mono(Wobj2, "right")
}
\keyword{manip}
\keyword{utilities}

