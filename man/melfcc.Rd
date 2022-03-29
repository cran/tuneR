\name{melfcc}
\alias{melfcc}
\title{ MFCC Calculation }
\description{
  Calculate Mel-frequency cepstral coefficients.
}
\usage{
melfcc(samples, sr = samples@samp.rate, wintime = 0.025, 
    hoptime = 0.01, numcep = 12, lifterexp = 0.6, htklifter = FALSE,
    sumpower = TRUE, preemph = 0.97, dither = FALSE,
    minfreq = 0, maxfreq = sr/2, nbands = 40, bwidth = 1, 
    dcttype = c("t2", "t1", "t3", "t4"), 
    fbtype = c("mel", "htkmel", "fcmel", "bark"), usecmp = FALSE, 
    modelorder = NULL, spec_out = FALSE, frames_in_rows = TRUE)
}
\arguments{
  \item{samples}{ Object of \link{Wave-class} or \link{WaveMC-class}. Only the first channel will be used.}
  \item{sr}{ Sampling rate of the signal. }
  \item{wintime}{ Window length in sec. }
  \item{hoptime}{ Step between successive windows in sec. }
  \item{numcep}{ Number of cepstra to return. }
  \item{lifterexp}{ Exponent for liftering; 0 = none. }
  \item{htklifter}{ Use HTK sin lifter. }
  \item{sumpower}{ If \code{sumpower = TRUE} the frequency scale transformation is based on the
    powerspectrum, if \code{sumpower = FALSE} it is based on its squareroot (absolute value of the spectrum) and
    squared afterwards. 
    %\code{sumpower=TRUE}: \code{sum(abs(fft)^2)}; \code{sumpower=FALSE}: \code{sum(abs(fft))}
  }
  \item{preemph}{ Apply pre-emphasis filter [1 -preemph] (0 = none). }
  \item{dither}{ Add offset to spectrum as if dither noise. }
  \item{minfreq}{ Lowest band edge of mel filters (Hz). }
  \item{maxfreq}{ Highest band edge of mel filters (Hz). }
  \item{nbands}{ Number of warped spectral bands to use. }
  \item{bwidth}{ Width of spectral bands in Bark/Mel. }
  \item{dcttype}{ Type of DCT used - 1 or 2 (or 3 for HTK or 4 for feacalc). }
  \item{fbtype}{ Auditory frequency scale to use: \code{"mel"},
    \code{"bark"}, \code{"htkmel"}, \code{"fcmel"}. }
  \item{usecmp}{ Apply equal-loudness weighting and cube-root compression (PLP
instead of LPC). }
  \item{modelorder}{ If \code{modelorder > 0}, fit a linear prediction (autoregressive-) model of this order and
calculation of cepstra out of \code{lpcas}. }
  \item{spec_out}{ Should matrices of the power- and the auditory-spectrum be
    returned.}
  \item{frames_in_rows}{ Return time frames in rows instead of columns (original Matlab code).}
}
\details{
Calculation of the MFCCs imlcudes the following steps:
\enumerate{
  \item{Preemphasis filtering}
  \item{Take the absolute value of the STFT (usage of Hamming window)}
  \item{Warp to auditory frequency scale (Mel/Bark)}
  \item{Take the DCT of the log-auditory-spectrum}
  \item{Return the first \sQuote{ncep} components}
  }
}
\value{
  \item{cepstra }{Cepstral coefficients of the input signal (one time frame per
row/column)}
  \item{aspectrum }{Auditory spectrum (spectrum after transformation to Mel/Bark
scale) of the signal}
  \item{pspectrum }{Power spectrum of the input signal.}
  \item{lpcas }{If \code{modelorder > 0}, the linear prediction coefficients (LPC/PLP).}
}
\references{Daniel P. W. Ellis: \url{https://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\note{
The following non-default values nearly duplicate Malcolm Slaney's mfcc (i.e. 
\preformatted{
melfcc(d, 16000, wintime=0.016, lifterexp=0, minfreq=133.33, 
       maxfreq=6855.6, sumpower=FALSE)
}
=~= \code{log(10) * 2 * mfcc(d, 16000)} in the Auditory toolbox for Matlab).

The following non-default values nearly duplicate HTK's MFCC (i.e. 
\preformatted{
melfcc(d, 16000, lifterexp=22, htklifter=TRUE, nbands=20, maxfreq=8000, 
    sumpower=FALSE, fbtype="htkmel", dcttype="t3")
} 
=~= \code{2 * htkmelfcc(:,[13,[1:12]])}
where HTK config has \sQuote{PREEMCOEF = 0.97}, \sQuote{NUMCHANS = 20},
\sQuote{CEPLIFTER = 22}, \sQuote{NUMCEPS = 12}, \sQuote{WINDOWSIZE = 250000.0}, \sQuote{USEHAMMING = T},
\sQuote{TARGETKIND = MFCC_0}).

For more detail on reproducing other programs' outputs, see
\url{https://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/mfccs.html}
}
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  m1 <- melfcc(testsound)

  #Use PLP features to calculate cepstra and output the matrices like the
  #original Matlab code (note: modelorder limits the number of cepstra)
  m2 <- melfcc(testsound, numcep=9, usecmp=TRUE, modelorder=8, 
    spec_out=TRUE, frames_in_rows=FALSE)
}
\keyword{ts}
\concept{mfcc}
\concept{lpc}
\concept{plp}
\concept{mel}
\concept{cepstrum}

