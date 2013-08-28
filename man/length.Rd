\name{length}
\docType{methods}
\alias{length,Wave-method}
\alias{length,WaveMC-method}
\alias{length,ANY-method}
\title{S4 generic for length}
\description{S4 generic for length.}
\section{Methods}{
\describe{
\item{x = "Wave"}{The length of the left channel (in samples) of this object of class \code{\link{Wave}} will be returned.}
\item{x = "WaveMC"}{The length for each of the time series in the \code{\link{WaveMC}} will be returned.}
\item{object = "ANY"}{For compatibility.}
}}
\seealso{For the primitive: \code{\link[base]{length}}}
\keyword{methods}
