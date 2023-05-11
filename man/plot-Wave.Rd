\name{plot-Wave}
\docType{methods}
\alias{plot,Wave,missing-method}
\alias{plot,WaveMC,missing-method}
\alias{plot_Wave_channel}
\title{Plotting Wave objects}
\description{Plotting objects of class \code{Wave}.}
\usage{
\S4method{plot}{Wave,missing}(x, info = FALSE, xunit = c("time", "samples"), 
    ylim = NULL, main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    simplify = TRUE, nr = 2500, axes = TRUE, yaxt = par("yaxt"), las = 1, 
    center = TRUE, ...)

\S4method{plot}{WaveMC,missing}(x, info = FALSE, xunit = c("time", "samples"), 
    ylim = NULL, main = NULL, sub = NULL, xlab = NULL, ylab = colnames(x), 
    simplify = TRUE, nr = 2500, axes = TRUE, yaxt = par("yaxt"), las = 1, 
    center = TRUE, mfrow = NULL, ...)
    
plot_Wave_channel(x, xunit, ylim, xlab, ylab, main, nr, simplify, axes = TRUE, 
    yaxt = par("yaxt"), las = 1, center = TRUE, ...)
}
\arguments{
  \item{x}{Object of class \code{\link{Wave}} or \code{\link{WaveMC}}, respectively.}
  \item{info}{Logical, whether to include (written) information on the \code{\link{Wave}} or \code{\link{WaveMC}} object within the plot.}
  \item{xunit}{Character indicating which units are used for setting up user coordinates (see \code{\link{par}})
    and x-axis labeling. If \code{xunit = "time"}, the unit is time in seconds, otherwise the number of samples.}
  \item{ylim}{The \code{y} (amplitude) limits of the plot.}
  \item{main, sub}{A title / subtitle for the plot.}
  \item{xlab}{Label for x-axis.}
  \item{ylab}{Label for y-axis (on the right side of the plot). For \code{WaveMC} objects, this can be the default \code{colnames(x)} 
      (i.e. channel names of the \code{WaveMC} object), \code{NULL} for \dQuote{channel 1}, \ldots, \dQuote{channel nc} 
      where nc is \code{ncol(x)}, \code{NA} for no labels, or a character vector of labels (one element for each channel).
      For \code{Wave} objects, this can be de default \dQuote{left channel} (for mono) or 
      \dQuote{left channel} and \dQuote{right channel} (for stereo), \code{NA} for no labels, 
      or a character vector of labels (one element for each channel).}
  \item{simplify}{Logical, whether the plot should be \dQuote{simplified}.
    If \code{TRUE} (default), not all (thousand/millions/billions) of points (samples) of the 
    \code{\link{Wave}} or \code{\link{WaveMC}} object are drawn, 
    but the \code{nr} (see below) ranges (in form of segments) within \code{nr} windows of the time series.
    
    Plotting with \code{simplify = FALSE} may take several minutes (depending on the number of samples in the \code{Wave} or \code{WaveMC})
    and output in any vector format may be really huge.}
  \item{nr}{Number of windows (segments) to be used \emph{approximately}
        (an appropriate number close to \code{nr} is selected)
        to \code{simplify} (see above) the plot.
        Only used if \code{simplify = TRUE} and the number of samples of the \code{Wave} or \code{WaveMC} object \code{x} is larger.}
  \item{axes}{Whether to plot axes, default is \code{TRUE}.}
  \item{yaxt}{How to plot the y-axis (\code{"n"} for no y-axis).} 
  \item{las}{The style of the axis labels, default is \code{las = 1} (always horizontal), see \code{\link{par}} for details.}
  \item{center}{Whether to plot with y-axes centered around 0 (or 127 if 8-bit), default is \code{TRUE}.}
  \item{mfrow}{A vector indicating the arrangement of the figures, see \code{\link{par}} for details.}
  \item{\dots}{Further arguments to be passed to the underlying plot functions.}
}
\details{Function \code{plot_Wave_channel} is a helper function
    to plot a single channel (left for a \code{Wave} object, first channel / first column of data slot of a \code{WaveMC} object); 
    in particular it is \emph{not} intended to be called by the user directly.}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}} and \link{tuneR}}
\keyword{hplot}
\keyword{aplot}
\keyword{methods}
\concept{Wave}
\concept{music}
\concept{speech}
