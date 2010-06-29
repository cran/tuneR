\name{freqconv}
\alias{bark2hz}
\alias{hz2bark}
\alias{mel2hz}
\alias{hz2mel}
\title{ Frequency scale conversion }
\description{
Perform frequency scale conversions between Hertz, Bark- and different variants
von the Melscale.
}
\usage{
bark2hz(z)
hz2bark(f)
hz2mel(f, htk = FALSE)
mel2hz(z, htk = FALSE)
}
\arguments{
  \item{f}{ Frequency in Hertz }
  \item{z}{ Frequency in the auditory frequency scale }
  \item{htk}{ Use the HTK-Melscale (\code{htk=TRUE}) or Slaney's Melscale from the
Auditory Toolbox (\code{htk=FALSE}) }
}
\value{
The value of the input in the target frequency scale.
}
\references{Daniel P. W. Ellis:
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Malcolm
Slaney: Auditory Toolbox }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\examples{
hz2bark(440)
bark2hz(hz2bark(440))
hz2mel(440, htk = TRUE)
mel2hz(hz2mel(440, htk = TRUE), htk = TRUE)
hz2mel(440, htk = FALSE)
mel2hz(hz2mel(440, htk = FALSE), htk = FALSE)
}
\concept{ mel }
\concept{ bark }
\concept{ hertz }
\concept{ frequency }
\concept{ conversion }
