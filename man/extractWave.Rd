\name{extractWave}
\alias{extractWave}
\title{Extractor for Wave objects}
\description{Extractor function that allows to extract inner parts for \code{Wave} objects (interactively).}
\usage{
extractWave(object, from = 1, to = length(object@left), 
    interact = interactive(), xunit = c("samples", "time"), ...)
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{from}{Sample number or time in seconds (see \code{xunit}) at which to \emph{start} extraction.}
  \item{to}{Sample number or time in seconds (see \code{xunit}) at which to \emph{stop} extraction.
    If \code{to < from}, \code{object} will be returned as is.}
  \item{interact}{Logical indicating whether to choose the range to be extracted interactively (if \code{TRUE}). 
    See Section Details.}
  \item{xunit}{Character indicating which units are used to specify the range to be extracted
    (both in arguments \code{from} and \code{to}, and in the plot, if \code{interact = TRUE}).
    If \code{xunit = "time"}, the unit is time in seconds, otherwise the number of samples.}
  \item{\dots}{Parameters to be passed to the underlying plot function (\code{\link{plot-methods}}), 
    if \code{interact = TRUE}.} 
}
\details{
This function allows interactive selection of a range to be extracted from an object of class \code{\link{Wave}}.
The default is to use interactive selection, if the current R session is \code{\link{interactive}}.
In case of interactive selection, \code{\link{plot-methods}} plot the \code{\link{Wave}} object,
and the user may click on the starting and ending points of his selection (given neither \code{from} nor \code{to}
have been specified, see below). 
The cut-points are drawn and the corresponding selection will be returned in form of a \code{\link{Wave}} object.

Setting \code{interact = TRUE} in a non-interactive session does not work.

Setting arguments \code{from} or \code{to} explicitly means that the specified one 
    does not need to be selected interactively, hence only the non-specified one will be selected interactively.
Moreover, setting both \code{from} or \code{to} implies \code{interact = FALSE}.
}
\value{
    An object of class \code{\link{Wave}}.
}
\examples{
Wobj <- sine(440, bit = 16)
# extracting the middle 0.5 seconds of that 1 sec. sound:
Wobj2 <- extractWave(Wobj, from = 0.25, to = 0.75, xunit = "time")
Wobj2

\dontrun{
# or interactively:
Wobj2 <- extractWave(Wobj)
}
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \code{\link{bind}}, \code{\link{channel}}, \code{\link{mono}}}
\keyword{manip}
\keyword{utilities}
\keyword{iplot}
\concept{Wave}
