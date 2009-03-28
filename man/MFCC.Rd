\name{MFCC}
\alias{MFCC}
\title{Mel Frequency Cepstral Coefficients}
\description{Computation of MFCCs (Mel Frequency Cepstral Coefficients) for a \code{Wave} object.}
\usage{
MFCC(object, a = 0.1, HW.width = 0.025, HW.overlapping = 0.25, 
    T.number = 24, T.overlapping = 0.5, K = 12)
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{a}{Coefficient for a first oder diffenrence filter, which is used to pre-emphasize the signal in first step of feature extraction.}
  \item{HW.width}{Width of Hamming window in seconds, which is used to divide the signal into frames.}
  \item{HW.overlapping}{Fraction  of how much the Hamming windows should overlap.}
  \item{T.number}{Number of triangular channels on the mel scaled spectrum, which are mapped to the signal.}
  \item{T.overlapping}{Fraction of how much the triangular filters should overlap.} 
  \item{K}{Number of desired output quefrencies the inverse discrete cosine transformation.}
}
\details{
This function computes Mel Frequency Cepstral Coefficients (MFCC) for an object of class \code{\link{Wave}}. 
In speech recognition MFCCs are used to extract the stimulus of the vocal tract from speech.
The process to create the MFCC features consist of five steps. 
First the signal from \code{object} is filtered with a finite impulse response (FIR) filter to pre-amplify high frequencies. 
Only the left channel of \code{object}, i.e. a mono signal, is used for the extraction. 
The parameter \code{a} controls the FIR filter. 
The filtered signal \eqn{S.fil} at time \eqn{t} is obtained by \eqn{S.fil(t) = S(t) - a*S(t-1)}.
In a second step the signal is converted to frames, each of length \code{HW.width}. 
A Hamming window is used to avoid any negative effects on the edges of each frame due to the conversion.
After a discrete Fourier transformation (DFT) the signal is mapped to the Mel scale filter bank. 
The filter bank consists of \code{T.number} triangular filters, which overlap by \code{T.overlapping}. 
This performs a perceptual weighting of frequeies.
In a last step an inverse discrete cosine transformation is applied to the signal. 
\code{K} controls the order, up to which MFCC features are computed.
}
\value{
A matrix (number of Hamming windows)-rows and \code{K+1} columns. 
The first columns is the energy, the follwing \code{K} columns the extracted MFCC features.
}
\note{This function is still in development and highly EXPERIMENTAL!!!}
\examples{
obj <- sine(440, bit = 16, duration = 5000)
MFCC(obj)
}
\references{Young, S., Everman, G., Gales, M., Hain, T., Kershaw, D., Moore, G., Odell, J., 
    Ollason, D., Povey, D., Valtchev, V., and Woodland, P. (2005): 
    \emph{The HTK-Book (v 3.3)}, Cambridge University Engineering Dept., 59-61.}
\author{Julia Schiffner \email{schiffner@statistik.tu-dortmund.de} and Gero Szepannek \email{szepannek@statistik.tu-dortmund.de} and Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{Wave}}}
\keyword{ts}
\concept{MFCC}
\concept{Mel}
\concept{Cepstrum}
