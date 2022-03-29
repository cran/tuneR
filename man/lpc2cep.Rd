\name{lpc2cep}
\alias{lpc2cep}
\title{ LPC to cepstra conversion }
\description{
 Convert the LPC coefficients in each column of \code{a} into frames of cepstra.
}
\usage{
lpc2cep(a, nout = nrow(a))
}
\arguments{
  \item{a}{ Matrix of LPC coefficients. }
  \item{nout}{ Number of cepstra to produce. }
}
\value{
Matrix of cepstra (one column per time frame).
}
\references{Daniel P. W. Ellis: \url{https://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{\code{\link{spec2cep}}}
%%, \code{\link{cep2spec}},  \code{\link{lpc2spec}} }
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)
  lpcas <- dolpc(aspectrum$aspectrum, 8)
  cepstra <- lpc2cep(lpcas)
}
\concept{ lpc }
\concept{ cepstra }
\concept{ conversion }
