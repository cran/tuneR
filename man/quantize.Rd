\name{quantize}
\alias{quantMerge}
\alias{quantize}
\title{Functions for the quantization of notes}
\description{These functions apply (static) quantization of notes in order 
    to produce sheet music by pressing the notes into bars.}
\usage{
quantize(notes, energy, parts)
quantMerge(notes, minlength, barsize, bars)
}
\arguments{
  \item{notes}{Series of notes, a vector of integers such as returned by \code{\link{noteFromFF}}.
    At least one argument (\code{notes} and/or \code{energy}) must be specified.}
  \item{energy}{Series of energy values, a vector of numerics such as corresponding components of a 
    \code{\link{Wspec}} object.}
  \item{parts}{Number of outcoming parts. The \code{notes} vector is divided into \code{parts} bins,
    the outcome is a vector of the modes of all bins.}
  \item{minlength}{1/(length of the shortest note).\cr
    Example: if the shortest note is a quaver (1/8), set \code{minlength = 8}.} 
  \item{barsize}{One bar contains \code{barsize} number of notes of length \code{minlength}.}
  \item{bars}{We expect \code{bars} number of bars.}
}
\value{
    \code{quantize} returns a list with components:
    \item{notes}{Vector of length \code{parts} corresponding to the input data
        The data is binned and modes corresponding to the data in those bins are returned.}
    \item{energy}{Same as \code{notes}, but for the \code{energy} argument.}\cr
    
    \code{quantMerge} returns a data.frame with components:
  \item{note}{integer representation of a note (see Arguments).}
  \item{duration}{1/duration of a note (see \code{minlength} in Section Arguments),
    if \code{punctuation = FALSE}.}
  \item{punctuation}{Whether the note should be punctuated. If \code{TRUE}, 
    the real duration is 1.5 times the duration given in \code{duration}.}
  \item{slur}{currently always \code{FALSE}, sensible processing is not yet implemented.\cr
    It is supposed to indicate the beginning and ending positions of slurs.}
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{to get the input: \code{\link{noteFromFF}}, 
    for plotting: \code{\link{quantplot}}, 
    for further processing: \code{\link{lilyinput}},
    to get notenames: \code{\link{notenames}};
    for an example, see the help in \link{tuneR}.}
\keyword{utilities}
\concept{quantization}
\concept{bin}
\concept{bar}
\concept{note}
