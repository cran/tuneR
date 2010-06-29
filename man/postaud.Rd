\name{postaud}
\alias{postaud}
\title{Equal loudness compression }
\description{
Do loudness equalization and cube root compression
}
\usage{
postaud(x, fmax, fbtype = c("bark", "mel", "htkmel", "fcmel"), 
    broaden = FALSE)
}
\arguments{
  \item{x}{ Matrix of spectra (output of \code{\link{audspec}}). }
  \item{fmax}{ Maximum frequency im Hertz. }
  \item{fbtype}{ Auditory frequency scale. }
  \item{broaden}{ Use two additional frequency bands for calculation. }
}
\value{
  \item{x }{Matrix of the per sample/frame (columns) spectra after applying the frequency
dependant loudness equalization and compression. }
  \item{eql }{Vector of the equal loudness curve. }
}
\references{Daniel P. W. Ellis
\url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}, Hynek
Hermansky }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{ \code{\link{audspec}}, \code{\link{dolpc}}} 
%% \code{\link{rastaplp}}
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)
  paspectrum <- postaud(x = aspectrum$aspectrum, fmax = 5000, 
    fbtype = "mel")
}
\concept{ plp }
\concept{ loudness }
\concept{ compression }
