\name{lilyinput}
\alias{lilyinput}
\encoding{latin1}
\title{Providing LilyPond compatible input}
\description{A function \emph{(in development!)} that writes a file to be processed by \emph{LilyPond}
    by extracting the relevant information (e.g. pitch, length, ...) from columns of a data frame.
    The music notation software \emph{LilyPond} can \dQuote{transcribe} such an input file into sheet music.
}
\usage{
lilyinput(X, file = "Rsong.ly", Major = TRUE, key = "c", 
    clef = c("treble", "bass", "alto", "tenor"), time = "4/4", 
    endbar = TRUE, midi = TRUE, tempo = "2 = 60", 
    textheight = 220, linewidth = 150, indent = 0, fontsize = 14)
}
\arguments{
  \item{X}{A data frame containing 4 named components (columns):
    \itemize{
        \item \code{note}: Integer - the notes' pitch in halftones from diapason (a),
            i.e. 0 for diapason a, 3 for c', ...
        \item \code{length}: Integer - denominator of lengths of the notes, e.g. 8 for a quaver.
        \item \code{punctate}: Logical - whether to punctate a note.
        \item \code{slur}: Logical - \code{TRUE} indicates to start a slur, or to end it.
            That means that the first, third, ... occurences of \code{TRUE} start slurps,
            while the second, fourth, ... occurences end slurps.
            Note that it is only possible to draw one slur at a time.
    }
  }
  \item{file}{The file to be written for \emph{LilyPond}'s input.}
  \item{Major}{Logical indicating major key (if \code{TRUE}) or minor key.}
  \item{key}{Keynote, necessary to set sharps/flats.}
  \item{clef}{Integer indicating the kind of clef, supported are \sQuote{treble} (default),
              \sQuote{bass}, \sQuote{alto}, and \sQuote{tenor}.}
  \item{time}{Character indicating which meter to use, examples are: \code{"3/4"}, \code{"4/4"}.}
  \item{endbar}{Logical indicating whether to set an ending bar at the end of the sheet music.}
  \item{midi}{Logical indicating whether Midi output (by \emph{LilyPond}) is desirable.}
  \item{tempo}{Character specifying the tempo to be used for the Midi file if \code{midi = TRUE}.
    The default, \code{"2 = 60"} indicates: 60 half notes per minute, whereas \code{"4 = 90"}
    indicates 90 quarters per minute.}
  \item{textheight}{Textheight of the sheet music to be written by \emph{LilyPond}.}
  \item{linewidth}{Linewidth of the sheet music to be written by \emph{LilyPond}.}
  \item{indent}{Indentation of the sheet music to be written by \emph{LilyPond}.}
  \item{fontsize}{Fontsize of the sheet music to be written by \emph{LilyPond}.}  
}
\details{
    Details will be given when development has reached a stable stage ...!
}
\value{
    Nothing is returned, but a \code{file} is written.
}
\references{
    The LilyPond development team (2005):
    \emph{LilyPond - The music typesetter}.
    \url{http://www.lilypond.org/}, Version 2.7.20.

    Preußer, A., Ligges, U. und Weihs, C. (2002): 
    \emph{Ein R Exportfilter für das Notations- und Midi-Programm LilyPond}.   
    Arbeitsbericht 35. Fachbereich Statistik, Universität Dortmund. (german)
}
\author{Andrea Preußer and Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\note{
    This function is in development!!!\cr
    Everything (and in particular its user interface) is subject to change!!!
}
\seealso{\code{\link{quantMerge}} prepares the data to be written into the LilyPond format;
    \code{\link{quantize}} and \code{\link{quantplot}} generate another kind of plot;
    and exhaustive example is given in \link{tuneR}.}
\keyword{file}
\keyword{interface}
\concept{LilyPond}
\concept{transcription}
\concept{transcribe}
\concept{note}
