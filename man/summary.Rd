\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,Wave-method}
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
\item{object = "Wave"}{The \code{\link{Wave}} object is beeing \code{show}ed, and an additional summary of the 
    \code{Wave}-object's (one or two) channels is given.}
\item{object = "Wspec"}{The \code{\link{Wspec}} object is beeing \code{show}ed, and as an additional output is given:
    \code{df}, \code{taper} (see \code{\link{spectrum}}), 
    and for the underlying \code{\link{Wave}} object the number of channels and its sampling rate.}
\item{object = "WspecMat"}{The \code{\link{WspecMat}} object is beeing \code{show}ed, and as an additional output is given:
    \code{df}, \code{taper} (see \code{\link{spectrum}}), 
    and for the underlying \code{\link{Wave}} object the number of channels and its sampling rate.}
}}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{For the S3 generic: \code{\link[base]{summary.default}}, \code{\link{plot-methods}}, 
    \code{\link{Wave}}, \code{\link{Wspec}}, \code{\link{WspecMat}}}
\keyword{methods}
\keyword{print}

