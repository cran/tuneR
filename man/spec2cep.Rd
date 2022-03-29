\name{spec2cep}
\alias{spec2cep}
\title{ Spectra to Cepstra Conversion }
\description{
Calculate cepstra from spectral samples (in columns of spec) through Discrete
Cosine Transformation.
}
\usage{
spec2cep(spec, ncep = 12, type = c("t2", "t1", "t3", "t4"))
}
\arguments{
  \item{spec}{ Input spectra (samples/time frames in columns).}
  \item{ncep}{ Number of cepstra to return. }
  \item{type}{ DCT Type. }
}
\value{
  \item{cep }{Matrix of resulting cepstra.}
  \item{dctm }{Returns the DCT matrix that spec was multiplied by to give cep.}
}
\references{Daniel P. W. Ellis: \url{https://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/} }
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\seealso{\code{\link{lpc2cep}}} 
%% \code{\link{cep2spec}}, \code{\link{lpc2spec}}
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  pspectrum <- powspec(testsound@left, testsound@samp.rate)
  aspectrum <- audspec(pspectrum, testsound@samp.rate)
  cepstra <- spec2cep(aspectrum$aspectrum)
}
\concept{ cepstra }
\concept{ spectra }
\concept{ conversion }
