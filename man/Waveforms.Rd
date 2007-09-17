\name{Waveforms}
\alias{Waveforms}
\alias{sine}
\alias{sawtooth}
\alias{square}
\alias{silence}
\alias{noise}
\title{Create Wave Objects of Special Waveforms}
\description{Create a \code{\link{Wave}} object of special waveform such as
silcence, (white/pink) noise, sawtooth, sine, and square.}
\usage{
noise(kind = c("white", "pink"), duration = samp.rate, 
        samp.rate = 44100, bit = 1, stereo = FALSE, 
        xunit = c("samples", "time"), ...)

sawtooth(freq, duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
        stereo = FALSE, xunit = c("samples", "time"), reverse = FALSE, ...)

silence(duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
        stereo = FALSE, xunit = c("samples", "time"), ...)

sine(freq, duration = samp.rate, from = 0, samp.rate = 44100, bit = 1,
        stereo = FALSE, xunit = c("samples", "time"), ...)

square(freq, duration = samp.rate, from = 0, samp.rate = 44100, bit = 1, 
        stereo = FALSE, xunit = c("samples", "time"), up = 0.5, ...)
}
\arguments{
  \item{kind}{The kind of noise, either \dQuote{white} or \dQuote{pink} 
    (the latter is not dB adjusted (!) but linear decreasing on a log-log scale).}
  \item{freq}{The frequency (in Hertz) to be generated.}
  \item{duration}{Duration of the \code{Wave} in \code{xunit}.}
  \item{from}{Starting value of the \code{Wave} in \code{xunit}.}
  \item{samp.rate}{Sampling rate of the \code{Wave}.}
  \item{bit}{Resolution of the \code{Wave} and rescaling unit. This may be\cr
        \code{1} (default) for rescaling to real values in [-1,1],\cr
        \code{8} (i.e. 8-bit) for rescaling to integers in [0, 254],\cr
        \code{16} (i.e. 16-bit) for rescaling to integers in [-32767, 32767],\cr
        \code{24} (i.e. 24-bit) for rescaling to integers in [-8388607, 8388607],\cr
        \code{32} (i.e. 32-bit) for rescaling to integers in [-2147483647, 2147483647], and\cr
        \code{0} for not rescaling at all. These numbers are internally passed to \code{\link{normalize}}.
        
        The \code{Wave} slot \code{bit} will be set to 8, if \code{bit=8}, and to 16 otherwise.}  
  \item{stereo}{Logical, if \code{TRUE}, a stereo sample will be generated.
    The right channel is identical to the left one for \code{sawtooth}, \code{silence},
    \code{sine}, and \code{square}. For \code{noise}, both channel are independent.}
  \item{xunit}{Character indicating which units are used
    (both in arguments \code{duration} and \code{from}).
    If \code{xunit = "time"}, the unit is time in seconds, otherwise the number of samples.}
  \item{reverse}{Logical, if \code{TRUE}, the waveform will be mirrored vertically.}
  \item{up}{A number between 0 and 1 giving the percentage of the waveform at max value 
    (= 1 - percentage of min value).}
  \item{...}{Further arguments to be passed to \code{\link{Wave}} through the internal function \code{postWaveform}.}
}
\value{
  A \code{\link{Wave}} object.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de},
    partly based on code from Matthias Heymann's former package \sQuote{sound},
    code for pink noise adapted and simplified from C code of Steve Moshier.}
\seealso{\code{\link{Wave}}, \code{\link{normalize}}, \code{\link{noSilence}}}
\examples{
Wobj <- sine(440, bit = 16, duration = 1000)
Wobj2 <- noise(bit = 16, duration = 1000)
plot(Wobj)
plot(Wobj2)
}
\keyword{datagen}
\concept{Wave}
\concept{sound}
\concept{waveform}
\concept{silcence}
\concept{sample}

