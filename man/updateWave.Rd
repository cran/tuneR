\name{updateWave}
\alias{updateWave}
\title{Update old Wave objects for use with new versions of tuneR}
\description{Update old Wave objects generated with \pkg{tuneR} < 1.0.0 to the new class definition for use with new versions of the package.}
\usage{
updateWave(object)
}
\arguments{
  \item{object}{An object of \link{Wave-class}.}
}
\details{This function is only needed to convert \link{Wave-class} objects that have been saved with \pkg{tuneR} versions prior to 1.0-0 to match the new class definition.}
\value{An object of \link{Wave-class} as implemented in \pkg{tuneR} versions >= 1.0-0.}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\link{Wave-class}, \code{\link{Wave}}}
\examples{
x <- sine(440)
updateWave(x)
}

