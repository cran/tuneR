\name{prepComb}
\alias{prepComb}
\title{Preparing the combination/concatenation of Wave or WaveMC objects}
\description{Preparing objects of class \code{Wave} or class \code{WaveMC} for binding/combination/concatenation by
    removing small amounts at the beginning/end of the \code{Wave} or \code{WaveMC} in order to make the transition smooth by avoiding clicks.
}
\usage{
prepComb(object, zero = 0, where = c("both", "start", "end"))
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.}
  \item{zero}{The zero level (default: 0) at which ideal cut points are determined (see Details).
    A typical alternative would be 127 for 8 bit \code{\link{Wave}} or \code{\link{WaveMC}} objects.
    If \code{zero = NA}, the mean of the left Wave channel (for a \code{Wave} object) 
    or the mean of the first channel (for a \code{WaveMC} object) is taken as zero level.}
  \item{where}{One of \code{"both"} (default), \code{"start"}, or \code{"end"}
    indicating at \code{where} to prepare the \code{\link{Wave}} or \code{\link{WaveMC}} object for concatenation.}
}
\value{
    An object of class \code{\link{Wave}} or class \code{\link{WaveMC}}.
}
\details{This function is useful to prepare objects of class \code{Wave} or class \code{WaveMC} for binding/combination/concatenation.
    At the side(s) indicated by \code{where} small amounts of the \code{Wave} or \code{WaveMC} are removed
    in order to make the transition between two \code{Wave}s or \code{WaveMC}s smooth (avoiding clicks).
     
     This is done by dropping all values at the \emph{beginning} of a \code{Wave} or \code{WaveMC} before the first positive point 
     after the \code{zero} level is crossed from negative to positive.
     Analogously, at the \emph{end} of a \code{Wave} or \code{WaveMC} all points are cut after the last negative value 
     before the last \code{zero} level crossing from negative to positive.
}
\note{If stereo (for \code{Wave}), only the left channel is analyzed while the right channel will simply be cut at the same locations.
If multi channel  (for \code{WaveMC}), only the first channel is analyzed while all other channels will simply be cut at the same locations.}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg,
    based on code from Matthias Heymann's former package \sQuote{sound}.}
\seealso{\code{\link{bind}}, \link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, 
\code{\link{WaveMC}}, \code{\link{extractWave}}, and \code{\link{noSilence}} to cut off silence}
\examples{
Wobj1 <- sine(440, duration = 520)
Wobj2 <- extractWave(sine(330, duration = 500), from = 110, to = 500)
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
\concept{WaveMC}
