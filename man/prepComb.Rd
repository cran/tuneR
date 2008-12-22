\name{prepComb}
\alias{prepComb}
\title{Preparing the combination/concatenation of Wave objects}
\description{Preparing objects of class \code{Wave} for binding/combination/concatenation by
    removing small amounts at the beginning/end of the Wave in order to make the transition smooth by avoiding clicks.
}
\usage{
prepComb(object, zero = 0, where = c("both", "start", "end"))
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{zero}{The zero level (default: 0) at which ideal cut points are determined (see Details).
    A typical alternative would be 127 for 8 bit \code{\link{Wave}} objects.
    If \code{zero=NA}, the mean of the left Wave channel is taken as zero level.}
  \item{where}{One of \dQuote{both} (default), \dQuote{start}, or \dQuote{end}
    indicating at \code{where} to prepare the \code{\link{Wave}} object for concatenation.}
}
\value{
    An object of class \code{\link{Wave}}.
}
\details{This function is useful to prepare objects of class \code{Wave} for binding/combination/concatenation.
    At the side(s) indicated by \code{where} small amounts of the Wave are removed
    in order to make the transition between two Waves smooth (avoiding clicks).
     
     This is done by dropping all values at the \emph{beginning} of a Wave before the first positive point 
     after the \code{zero} level is crossed from negative to positive.
     Analogously, at the \emph{end} of a Wave all points are cut after the last negative value 
     before the last \code{zero} level crossing from negative to positive.
}
\note{If stereo, only the left channel is analyzed while the right channel will be simply cut at the same locations.}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}, 
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\code{\link{bind}}, \code{\link{Wave}}, \code{\link{extractWave}}, and \code{\link{noSilence}} to cut off silence}
\examples{
Wobj1 <- sine(440, duration = 520, bit = 16)
Wobj2 <- extractWave(sine(330, duration = 500, bit = 16), from = 110, to = 500)
par(mfrow = c(2,1))
plot(bind(Wobj1, Wobj2), xunit = "samples")
abline(v = 520, col = "red")  # here is a "click"!

# now remove the "click" by deleting a minimal amount of information:
Wobj1 <- prepComb(Wobj1, where = "end")
Wobj2 <- prepComb(Wobj2, where = "start")
plot(bind(Wobj1, Wobj2), xunit = "samples")
}
\keyword{manip}
\keyword{utilities}
\concept{declick}
\concept{Wave}
