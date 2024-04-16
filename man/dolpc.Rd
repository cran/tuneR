\name{dolpc}
\alias{dolpc}
\title{ (Perceptive) Linear Prediction }
\description{
Compute autoregressive model from spectral magnitude samples via Levinson-Durbin
recursion.
}
\usage{
dolpc(x, modelorder = 8)
}
\arguments{
  \item{x}{ Matrix of spectral magnitude samples (each sample/time frame in one
column). }
  \item{modelorder}{ Lag of the AR model. }
}
\value{
Returns a matrix of the normalized AR coefficients (depending on the input
spectrum: LPC or PLP coefficients). Every column represents one time frame.
}
\references{Daniel P. W. Ellis: \url{https://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link[signal]{levinson}} }
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)$aspectrum
  lpcas <- dolpc(aspectrum, 10)
}
\concept{ lpc }
\concept{ plp }
\concept{ levinson }
\concept{ durbin }
\concept{ recursion }
