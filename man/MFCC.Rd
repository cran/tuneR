\name{MFCC}
\alias{MFCC}
\title{DEPRECATED: Mel Frequency Cepstral Coefficients}
\description{DEPRECATED: Computation of MFCCs --- this has been replaced by \code{\link{melfcc}} already and is just a wrapper! Will be removed shortly.}
\usage{
MFCC(object, a = 0.1, HW.width = 0.025, HW.overlapping = 0.25, 
    T.number = 24, T.overlapping = NA, K = 12)
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{a}{Coefficient for a first oder diffenrence filter, which is used to pre-emphasize the signal in first step of feature extraction.}
  \item{HW.width}{Width of Hamming window in seconds, which is used to divide the signal into frames.}
  \item{HW.overlapping}{Fraction  of how much the Hamming windows should overlap.}
  \item{T.number}{Number of triangular channels on the mel scaled spectrum, which are mapped to the signal.}
  \item{T.overlapping}{IGNORED!} 
  \item{K}{Number of desired output quefrencies the inverse discrete cosine transformation.}
}
\note{This function was always documented to be in development and highly EXPERIMENTAL!!!}
\seealso{\link{melfcc}}
\keyword{ts}

