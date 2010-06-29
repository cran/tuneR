\name{levinson}
\alias{levinson}
\title{ Durbin-Levinson Recursion }
\description{
Internal: Perform Durbin-Levinson recursion on vector or matrix.
}
\usage{
levinson(x, p = NULL)
}
\arguments{
  \item{x}{ Input signal. }
  \item{p}{ Lag (defaults to \code{length(x)} or \code{nrow(x)}). }
}
\details{
 Use the Durbin-Levinson algorithm to solve:

    \code{toeplitz(acf(1:p)) * y = -acf(2:p+1).}

 The solution [1, y'] is the denominator of an all pole filter
 approximation to the signal x which generated the autocorrelation
 function acf.  

 acf is the autocorrelation function for lags 0 to p.
}
\value{
  \item{a }{The denominator filter coefficients.}
  \item{v }{Variance of the white noise = square of the numerator constant.}
  \item{ref }{Reflection coefficients = coefficients of the lattice
implementation of the filter.}
}
\references{
 Steven M. Kay and Stanley Lawrence Marple Jr.:
   \emph{Spectrum analysis -- a modern perspective},
   Proceedings of the IEEE, Vol 69, pp 1380-1419, Nov., 1981

 Octave \url{http://octave.sf.net}
}
\author{ Original Octave version by Paul Kienzle
  \email{pkienzle@users.sf.net}. Conversion to R by Sebastian Krey
\email{krey@statistik.tu-dortmund.de}. }
\keyword{internal}
\concept{ levinson }
\concept{ durbin }
\concept{ recursion }
