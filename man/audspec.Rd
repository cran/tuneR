\name{audspec}
\alias{audspec}
\title{Frequency band conversion}
\description{
Perform critical band analysis (see PLP), which means the reduction of the
fourier frequencies of a signal's powerspectrum to a reduced number of frequency
bands in an auditory frequency scale.
}
\usage{
audspec(pspectrum, sr = 16000, nfilts = ceiling(hz2bark(sr/2)) + 1, 
    fbtype = c("bark", "mel", "htkmel", "fcmel"), minfreq = 0, 
    maxfreq = sr/2, sumpower = TRUE, bwidth = 1)
}
\arguments{
  \item{pspectrum}{ Output of \code{\link{powspec}}, matrix with the powerspectrum of each time frame in its columns. }
  \item{sr}{ Sample rate of the original recording.  }
  \item{nfilts}{ Number of filters/frequency bins in the auditory frequency scale. }
  \item{fbtype}{ Used auditory frequency scale. }
  \item{minfreq}{ Lowest frequency. }
  \item{maxfreq}{ Highest frequency. }
  \item{sumpower}{ If \code{sumpower=TRUE} the frequency scale transformation is based on the
powerspectrum, if \code{sumpower=FALSE} it is based on its squareroot (absolute value of the spectrum) and
squared afterwards.}
  \item{bwidth}{ Modify the width of the frequency bands. }
}
\value{
  \item{aspectrum }{Matrix with the auditory spectrum of each time frame in its columns.}
  \item{wts }{Weight matrix for the frequency band conversion.}
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{\code{\link{fft2melmx}}, \code{\link{fft2barkmx}}}
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)
}
\concept{frequency}
\concept{conversion}
\concept{bark}
\concept{mel}
