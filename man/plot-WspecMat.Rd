\name{plot-WspecMat}
\docType{methods}
\alias{plot-WspecMat}
\alias{plot,WspecMat,missing-method}
\alias{image-methods}
\alias{image,ANY-method}
\alias{image-Wspec}
\alias{image,Wspec-method}
\title{Plotting WspecMat objects}
\description{Plotting a spectogram (image) of an object of class \code{Wspec} or \code{WspecMat}.}
\usage{
\S4method{plot}{WspecMat,missing}(x, xlab = "time", ylab = "Frequency", 
    xunit = c("samples", "time"), log = "", ...)
\S4method{image}{Wspec}(x, xlab = "time", ylab = "Frequency", 
    xunit = c("samples", "time"), log = "", ...)
}
\arguments{
  \item{x}{Object of class \code{\link{WspecMat}} (for \code{plot}) or \code{\link{Wspec}} (for \code{image}).}
  \item{xlab, ylab}{Label for x-/y-axis.}
  \item{xunit}{Character indicating which units are used to annotate the x axis.
    If \code{xunit = "time"}, the unit is time in seconds, otherwise the number of samples.}
  \item{log}{Character - \code{"z"} if the z values are to be logarithmic.}
  \item{\dots}{Further arguments to be passed to the underlying \code{\link{image}} function. 
    See \code{\link{image}} for details.}
}
\details{
Calling \code{image} on a \code{Wspec} object converts it to class 
\code{WspecMat} and calls the corresponding \code{plot} function.\cr
Calling \code{plot} on a \code{WspecMat} object
generates an \code{\link{image}} with correct annotated axes.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{see \code{\link{image}}, \code{\link{Wspec}}, \code{\link{WspecMat}}, \code{\link{periodogram}} and \link{tuneR} 
    for the constructor function and some examples.}
\keyword{hplot}
\keyword{methods}
\concept{periodogram}
\concept{spectogram}
\concept{Wave}
