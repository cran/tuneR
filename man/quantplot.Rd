\name{quantplot}
\alias{quantplot} 
\title{Plotting the quantization of a melody}
\description{Plot an observed melody and (optional) an expected melody, 
    as well as corresponding energy values (corresponding to the loudness of the sound)
    within a quantization grid.}
\usage{
quantplot(observed, energy = NULL, expected = NULL, bars, 
    barseg = round(length(observed) / bars), 
    main = NULL, xlab = NULL, ylab = "note", xlim = NULL, ylim = NULL, 
    observedcol = "red", expectedcol = "grey", gridcol = "grey",
    lwd = 2, las = 1, cex.axis = 0.9, mar = c(5, 4, 4, 4) + 0.1,
    notenames = NULL, silence = "silence", plotenergy = TRUE, ...,
    axispar = list(ax1 = list(side=1), ax2 = list(side=2), ax4 = list(side=4)),
    boxpar = list(), 
    energylabel = list(text="energy", side=4, line=2.5, at=rg.s-0.25, las=3),
    energypar = list(pch = 20), 
    expectedpar = list(),
    gridpar = list(gridbar = list(col = 1), gridinner = list(col=gridcol)),
    observedpar = list(col = observedcol, pch = 15))
}
\arguments{
  \item{observed}{Either a vector of observed notes resulting from some quantization, 
    or a list with components \code{notes} (observed notes) and \code{energy} (corresponding energy values),
    e.g. the result from a call to \code{\link{quantize}}.}
  \item{energy}{A vector of energy values with same quantization as \code{observed}     
    (overwrites any given energy values, if \code{observed} is a list).}
  \item{expected}{Expected notes (optional; in order to compare results).}
  \item{bars}{Number of bars to be plotted (e.g. corresponding to \code{quantize} arguments).}
  \item{barseg}{Number of segments (minimal length notes) in each bar.}
  \item{main}{Main title of the plot.}
  \item{xlab, ylab}{Annotation of x/y-axes.}
  \item{xlim, ylim}{Range of x/y-axis.}
  \item{observedcol}{Colour for the observed notes.}
  \item{expectedcol}{Colour for the expected notes.}
  \item{gridcol}{Colour of the inner-bar grid.}
  \item{lwd}{Line width, see \code{\link{par}} for details.}
  \item{las}{Orientation of axis labels, see \code{\link{par}} for details.}
  \item{cex.axis}{Size of tick mark labels, see \code{\link{par}} for details.}
  \item{mar}{Margins of the plot, see \code{\link{par}} for details.}
  \item{notenames}{Optionally specify other notenames (character) for the y axis.}
  \item{silence}{Character string for label of the \sQuote{silence} (default) axis.}
  \item{plotenergy}{Logical indicating whether to plot energy values in the bottom part of the plot;
    default is \code{TRUE}), if energy values are specified, and \code{FALSE} otherwise.}
  \item{...}{Additional graphical parameters to be passed to underlying \code{plot} function.}
  \item{axispar}{A named list of three other lists (\code{ax1},\code{ax2}, and \code{ax4})
    containing parameters passed to the corresponding \code{\link{axis}} calls for the three axis
    time (\code{ax1}), notes (\code{ax2}), and energy (\code{ax4}).}
  \item{boxpar}{A list of parameters to be passed to the box generating functions.}
  \item{energylabel}{A list of parameters to be passed to the energy-label 
    generating \code{\link{mtext}} call.}
  \item{energypar}{A list of parameters to be passed to the \code{\link{points}} function 
    that draws the energy values.}
  \item{expectedpar}{A list of parameters to be passed to the \code{\link{rect}} 
    function that draws the rectangles for expected values.}
  \item{gridpar}{A named list of two other lists (\code{gridbar} and \code{gridinner}) 
    containing parameters passed to the \code{\link{abline}} functions that draw the grid lines 
    (for bar separators and inner bar (note) separators).}
  \item{observedpar}{A list of parameters to be passed to the \code{\link{lines}} 
    function that draws the observed values.}
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\code{\link{noteFromFF}}, \code{\link{FF}}, \code{\link{melodyplot}}, \code{\link{quantize}}; 
    for an example, see the help in \link{tuneR}.}
\keyword{hplot}
\concept{melody}
\concept{note}
\concept{transcription}
\concept{tracking}
\concept{pitch}
