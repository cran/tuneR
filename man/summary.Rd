\name{summary-methods}
\docType{methods}
\alias{summary,ANY-method}
\alias{summary,Wave-method}
\alias{summary,WaveMC-method}
\alias{summary,Wspec-method}
\alias{summary,WspecMat-method}
\title{Object Summaries}
\description{
summary is a generic function used to produce result summaries of the results of various model fitting functions. 
The function invokes particular methods which depend on the class of the first argument. 
}
\section{Methods}{
\describe{
\item{object = "ANY"}{Any object for which a summary is desired, 
    dispatches to the S3 generic.}
\item{object = "Wave"}{The \code{\link{Wave}} object is being \code{show}n and an additional summary of the 
    \code{Wave}-object's (one or two) channels is given.}
\item{object = "WaveMC"}{The \code{\link{WaveMC}} object is being \code{show}n and an additional summary of the 
    \code{WaveMC}-object's channels is given.}
\item{object = "Wspec"}{The \code{\link{Wspec}} object is being \code{show}n and as an additional output is given:
    \code{df}, \code{taper} (see \code{\link{spectrum}}) 
    and for the underlying \code{\link{Wave}} object the number of channels and its sampling rate.}
\item{object = "WspecMat"}{The \code{\link{WspecMat}} object is being \code{show}n and as an additional output is given:
    \code{df}, \code{taper} (see \code{\link{spectrum}}) 
    and for the underlying \code{\link{Wave}} object the number of channels and its sampling rate.}
}}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{For the S3 generic: \code{\link[base]{summary.default}}, \code{\link{plot-methods}}, 
    \link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}, \code{\link{Wspec}}, \code{\link{WspecMat}}, \code{\link{show}}}
\keyword{methods}
\keyword{print}

