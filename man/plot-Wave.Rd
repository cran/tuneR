\name{plot-Wave}
\docType{methods}
\alias{plot,Wave,missing-method}
\alias{plot.Wave.channel}
\title{Plotting Wave objects}
\description{Plotting objects of class \code{Wave}.}
\usage{
\S4method{plot}{Wave,missing}(x, info = FALSE, xunit = c("time", "samples"), 
    ylim = NULL, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    simplify = TRUE, nr = 1500, ...)
    
plot.Wave.channel(x, xunit, ylim, xlab, ylab, main, nr, simplify, ...)
}
\arguments{
  \item{x}{Object of class \code{\link{Wave}}.}
  \item{info}{Logical, whether to include (written) information on the \code{\link{Wave}} object within the plot.}
  \item{xunit}{Character indicating which units are used for setting up user coodinates (see \code{\link{par}})
    and x-axis labeling. If \code{xunit = "time"}, the unit is time in seconds, otherwise the number of samples.}
  \item{ylim}{The \code{y} (amplitude) limits of the plot.}
  \item{main, sub}{A main / sub title for the plot.}
  \item{xlab, ylab}{Label for x-/y-axis.}
  \item{simplify}{Logical, whether the plot should be \dQuote{simplified}.
    If \code{TRUE} (default), not all (thousand/millions/billions) of points (samples) of the 
    \code{\link{Wave}} object are drawn, 
    but the \code{nr} (see below) ranges (in form of segments) within \code{nr} windows of the time series.
    
    Plotting with \code{simplify = FALSE} may take several minutes (depending on the number of samples in the \code{Wave})
    and output in any vector format may be really huge.}
  \item{nr}{Number of windows (segments) to be used \emph{approximately}
        (an appropriate number close to \code{nr} is selected)
        to \code{simplify} (see above) the plot.
        Only used if \code{simplify = TRUE} and the number of samples of \code{Wave} object \code{x} is larger.}
  \item{\dots}{Further arguments to be passed to the underlying plot functions.}
}
\details{Function \code{plot.Wave.channel} is a helper function
    to plot a single (left!) channel; in particular it is \emph{not} intended to be called by the user directly.}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{Wave}}}
\keyword{hplot}
\keyword{aplot}
\keyword{methods}
