\name{plot-Wspec}
\docType{methods}
\alias{plot-Wspec}
\alias{plot,Wspec,missing-method}
\title{Plotting Wspec objects}
\description{Plotting a periodogram contained in an object of class \code{Wspec}.}
\usage{
\S4method{plot}{Wspec,missing}(x, which = 1, type = "h", xlab = "Frequency", 
    ylab = NULL, log = "", ...)
}
\arguments{
  \item{x}{Object of class \code{\link{Wspec}}.}
  \item{which}{Integer indicating which of the periodograms contained in object \code{x} to plot.
    Default is to plot the first one.}
  \item{type}{The default is to plot horizontal lines, rather than points. See \code{\link{plot.default}} for details.}
  \item{xlab, ylab}{Label for x-/y-axis.}
  \item{log}{Character - \code{"x"} if the x-axis is to be logarithmic, 
    \code{"y"} if the y-axis is to be logarithmic (quite typical for some visualizations of periodograms), 
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}
  \item{\dots}{Further arguments to be passed to the underlying plot functions. 
    See \code{\link{plot.default}} for details.}
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{see \code{\link{Wspec}}, \code{\link{periodogram}} and \link{tuneR} 
    for the constructor function and some examples.}
\keyword{hplot}
\keyword{methods}
\concept{periodogram}
\concept{Wave}
