\name{MCnames}
\alias{MCnames}
\docType{data}
\title{Default channel ordering for multi channel wave files}
\description{A data frame representing the default channel ordering with id, descriptive label, and abbreviated name for multi channel wave files.}
\format{
  A data frame with 18 observations on the following 3 variables:
  \describe{
    \item{\code{id}}{id of the channel}
    \item{\code{label}}{full label for the channel}
    \item{\code{name}}{abbreviated name for the channel}
  }
}
\source{Data derived from the technical documentation given at \url{http://www.microsoft.com/whdc/device/audio/multichaud.mspx}.}
\references{Microsoft Corporation (2007): Multiple Channel Audio Data and WAVE Files, \url{http://www.microsoft.com/whdc/device/audio/multichaud.mspx}.}
\examples{
MCnames # the 18 predefined channels in a multi channel Wave file (WaveMC object)
}
\keyword{datasets}
