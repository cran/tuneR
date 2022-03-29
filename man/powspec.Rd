\name{powspec}
\alias{powspec}
\title{ Powerspectrum }
\description{
Compute the powerspectrum of the input signal.
Basically output a power spectrogram using a Hamming window.
}
\usage{
powspec(x, sr = 8000, wintime = 0.025, steptime = 0.01, dither = FALSE)
}
\arguments{
  \item{x}{ Vector of samples. }
  \item{sr}{ Sampling rate of the signal. }
  \item{wintime}{ Window length in sec. }
  \item{steptime}{ Step between successive windows in sec. }
  \item{dither}{ Add offset to spectrum as if dither noise. }
}
\value{
Matrix, where each column represents a power spectrum for a given frame
and each row represents a frequency.
}
\references{Daniel P. W. Ellis: \url{https://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link[signal]{specgram}} }
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
}
\concept{ powerspectrum }
