\name{show-WaveWspec-methods}
\docType{methods}
\alias{show,Wave-method}
\alias{show,Wspec-method}
\title{Showing objects}
\description{Showing \code{Wave} and \code{Wspec} objects.}
\section{Methods}{
\describe{
\item{object = "Wave"}{The \code{Wave} object is beeing \code{show}ed.
    The number of samples, duration in seconds, Samplingrate (Hertz),
    Stereo / Mono, and the resolution in bits are printed.
    Note that it does not make sense to print the whole channels 
    containing several thousands or millions of samples.}
\item{object = "Wspec"}{The number of periodograms, Fourier frequencies, window width (used amount of data), 
    amount of overlap of neighboring windows, and whether the periodogram(s) has/have been normalized
    will be printed.}
}}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{Wave}}, \code{\link{Wspec}}, \code{\link{plot-methods}}, \code{\link{summary-methods}},
    and \code{\link{periodogram}} for the constructor function and some examples}
\keyword{methods}
\keyword{print}
