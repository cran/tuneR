\name{fftfreqconvmx}
\alias{fft2barkmx}
\alias{fft2melmx}
\title{Internal: Convert FFT frequency bins to Bark/Mel bins }
\description{
 Generate a matrix of weights to combine FFT bins into Bark/Mel bins.
}
\usage{
fft2barkmx(nfft, sr = 8000, nfilts = NULL, width = 1, minfreq = 0, 
    maxfreq = sr/2)
    
fft2melmx(nfft, sr = 8000, nfilts = 40, width = 1, minfreq = 0, 
    maxfreq = sr/2, htkmel = FALSE, constamp = FALSE)
}
\arguments{
  \item{nfft}{ Source FFT size. }
  \item{sr}{ Sampling rate of the signal. }
  \item{nfilts}{ Number of desired output frequency bands. If \code{NULL} then one
    per Bark/Mel between \code{minfreq} and \code{maxfreq}. }
  \item{width}{ Width of each output frequency band in Bark/Mel. }
  \item{minfreq}{ Minimum frequency. }
  \item{maxfreq}{ Maximum frequency. }
  \item{htkmel}{ Use HTK- or Slaney's curve of the Melscale. }
  \item{constamp}{ Make integration windows peak at 1 (\code{constamp=TRUE}) or
    sum to 1 (\code{constamp=FALSE}) }
}
\value{
  \item{wts }{The weight matrix with 'nfft' columns and 'nfilts' rows.}
  \item{binfreqs }{Edge frequencies of the bins.}
}
\references{Daniel P. W. Ellis:
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Malcolm
Slaney: Auditory Toolbox
\url{https://engineering.purdue.edu/~malcolm/interval/1998-010/}}
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\note{
      While wts has nfft columns, the second half are all zero. 
      Hence, Bark spectrum is \code{fft2barkmx(nfft, sr) \%*\% abs(fft(xincols, nfft))}.
}
\seealso{ \code{\link{hz2bark}}, \code{\link{hz2mel}} }
\examples{
  #Mel matrix in Slaney's mfcc.m:
  #tuneR:::fft2melmx(512, 8000, 40, 1, 133.33, 6855.5, FALSE, FALSE)
}
\keyword{internal}
\concept{fft}
\concept{frequency}
\concept{bark}
\concept{mel}
\concept{conversion}
