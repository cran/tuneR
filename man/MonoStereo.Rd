\name{Mono-Stereo}
\alias{mono}
\alias{stereo}
\title{Converting (extracting, joining) stereo to mono and vice versa}
\description{Functions to extract a channel from a stereo \code{Wave} object,
    and to join channels of two monophonic \code{Wave} objects to a stereophonic one.}
\usage{
mono(object, which = c("left", "right", "both"))
stereo(left, right)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}}.}
    \item{which}{Character, indicating whether the \dQuote{left} or \dQuote{right} channel should be extracted,
        or whether \dQuote{both} channels should be averaged.}
    \item{left}{Object of class \code{\link{Wave}} containing \code{mono}phonic sound,
        to be used for the left channel.}
    \item{right}{Object of class \code{\link{Wave}} containing \code{mono}phonic sound,
        to be used for the right channel (if missing, the \code{left} channel is duplicated).
        If \code{right} is missing, \code{stereo} returns whether \code{left} is 
        stereo (\code{TRUE}) or mono (\code{FALSE}).}
}
\value{
    An object of class \code{\link{Wave}}.
    
    If argument \code{right} is missing in \code{stereo}, a logical values is returned 
    that indicates whether \code{left} is stereo (\code{TRUE}) or mono (\code{FALSE}).
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{Wave}}}
\examples{
Wobj <- sine(440, bit = 16)
Wobj
Wobj2 <- stereo(Wobj, Wobj)
Wobj2
mono(Wobj2, "right")
}
\keyword{manip}
\keyword{utilities}
\concept{Wave}
\concept{mono}
\concept{stereo}
\concept{channel}
