\name{melodyplot}
\alias{melodyplot} 
\title{Plotting a melody}
\description{Plot a observed melody and (optional) an expected melody, 
    as well as corresponding energy values (corresponding to the loudness of the sound).}
\usage{
melodyplot(object, observed, expected = NULL, bars = NULL, 
    main = NULL, xlab = NULL, ylab = "note", xlim = NULL, ylim = NULL, 
    observedcol = "red", expectedcol = "grey", gridcol = "grey",
    lwd = 2, las = 1, cex.axis = 0.9, mar = c(5, 4, 4, 4) + 0.1)
}
\arguments{
  \item{object}{An object of class \code{\link{Wspec}}.}
  \item{observed}{Observed notes, probably as a result from \code{\link{noteFromFF}} (or a smoothed version).
    This should correspond to the \code{\link{Wspec}} \code{object}.}
  \item{expected}{Expected notes (optional; in order to compare results).}
  \item{bars}{Number of bars to be plotted (a virtual static segmentation takes place).
    If \code{NULL} (default), time rather than bars are used.}
  \item{main}{Main title of the plot.}
  \item{xlab}{Annotation of x-axis.}
  \item{ylab}{Annotation of y-axis.}
  \item{xlim}{Range of x-axis.}
  \item{ylim}{Range of y-axis.}
  \item{observedcol}{Colour for the oberved melody.}
  \item{expectedcol}{Colour for the expected melody.}
  \item{gridcol}{Colour of the grid.}
  \item{lwd}{Line width, see \code{\link{par}} for details.}
  \item{las}{Orientation of axis labels, see \code{\link{par}} for details.}
  \item{cex.axis}{Size of tick mark labels, see \code{\link{par}} for details.}
  \item{mar}{Margins of the plot, see \code{\link{par}} for details.}
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{noteFromFF}}, \code{\link{FF}}}
\keyword{hplot}
