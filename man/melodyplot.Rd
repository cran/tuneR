\name{melodyplot}
\alias{melodyplot} 
\title{Plotting a melody}
\description{Plot a observed melody and (optional) an expected melody, 
    as well as corresponding energy values (corresponding to the loudness of the sound).}
\usage{
melodyplot(object, observed, expected = NULL, bars = NULL, 
    main = NULL, xlab = NULL, ylab = "note", xlim = NULL, ylim = NULL, 
    observedtype = "l", observedcol = "red", expectedcol = "grey", 
    gridcol = "grey", lwd = 2, las = 1, cex.axis = 0.9, 
    mar = c(5, 4, 4, 4) + 0.1, notenames = NULL, thin = 1, 
    silence = "silence", plotenergy = TRUE, ...,
    axispar = list(ax1 = list(side=1),
                   ax2 = list(side=2), 
                   ax4 = list(side=4)),
    boxpar = list(), 
    energylabel = list(text="energy", side=4, line=2.5, at=rg.s-0.25, las=3),
    energypar = list(), 
    expectedpar = list(),
    gridpar = list(col=gridcol), 
    observedpar = list(col=observedcol, type=observedtype, lwd=2, pch=15))
}
\arguments{
  \item{object}{An object of class \code{\link{Wspec}}.}
  \item{observed}{Observed notes, probably as a result from \code{\link{noteFromFF}} (or a smoothed version).
    This should correspond to the \code{\link{Wspec}} \code{object}.
    It can also be a matrix of \code{k} columns where those \code{k} notes in the same row 
    are displayed at the same timepoint.}
  \item{expected}{Expected notes (optional; in order to compare results), same format as \code{observed}.}
  \item{bars}{Number of bars to be plotted (a virtual static segmentation takes place).
    If \code{NULL} (default), time rather than bars are used.}
  \item{main}{Main title of the plot.}
  \item{xlab, ylab}{Annotation of -/y-axes.}
  \item{xlim, ylim}{Range of x-/y-axis, where \code{ylim} must be an integer 
    that represents the range of note heights that should be displayed.}
  \item{observedtype}{Type (either \code{"p"} for points or \code{"l"} for lines) 
    used for representing observed notes. 
    \code{"l"} (the default) is not sensible for polyphonic representations.}
  \item{observedcol}{Colour for the observed melody.}
  \item{expectedcol}{Colour for the expected melody.}
  \item{gridcol}{Colour of the grid.}
  \item{lwd}{Line width, see \code{\link{par}} for details.}
  \item{las}{Orientation of axis labels, see \code{\link{par}} for details.}
  \item{cex.axis}{Size of tick mark labels, see \code{\link{par}} for details.}
  \item{mar}{Margins of the plot, see \code{\link{par}} for details.}
  \item{notenames}{Optionally specify other notenames (character) for the y axis.}
  \item{thin}{Amount of thinning of notenames, i.e. only each \code{thin}th notename is displayed on the y-axis.}
  \item{silence}{Character string for label of the \sQuote{silence} (default) axis.}
  \item{plotenergy}{Logical (default: \code{TRUE}), 
    whether to plot energy values in the bottom part of the plot.}
  \item{...}{Additional graphical parameters to be passed to underlying \code{plot} function.}
  \item{axispar}{A named list of three other lists (\code{ax1}, \code{ax2}, and \code{ax4})
    containing parameters passed to the corresponding \code{\link{axis}} calls for the three axis
    time (\code{ax1}), notes (\code{ax2}), and energy (\code{ax4}).}
  \item{boxpar}{A list of parameters to be passed to the box generating functions.}
  \item{energylabel}{A list of parameters to be passed to the energy-label 
    generating \code{\link{mtext}} call.}
  \item{energypar}{A list of parameters to be passed to the \code{\link{lines}} function that draws the energy curve.}
  \item{expectedpar}{A list of parameters to be passed to the \code{\link{rect}} 
    function that draws the rectangles for expected values.}
  \item{gridpar}{A list of parameters to be passed to the \code{\link{abline}} 
    function that draws the grid lines.}
  \item{observedpar}{A list of parameters to be passed to the \code{\link{lines}} 
    function that draws the observed values.}
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{\code{\link{noteFromFF}}, \code{\link{FF}}, \code{\link{quantplot}}; 
    for an example, see the help in \link{tuneR}.}    
\keyword{hplot}
\concept{melody}
\concept{note}
\concept{transcription}
\concept{tracking}
\concept{pitch}
