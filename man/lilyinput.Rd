\name{lilyinput}
\alias{lilyinput}
\title{Providing LilyPond compatible input}
\description{A function \emph{(in development!)} that writes a file to be processed by \emph{LilyPond}
    by extracting the relevant information (e.g. pitch, length, ...) from columns of a data frame.
    The music notation software \emph{LilyPond} can \dQuote{transcribe} such an input file into sheet music.
}
\usage{
lilyinput(X, file = "Rsong.ly", Dur = TRUE, grundton = "c", 
    schlusselart = 1, takt = "4/4", endbar = TRUE, midi = TRUE, 
    tempo = "2 = 60", textheight = 220, linewidth = 150, indent = 0)
}
\arguments{
  \item{X}{A data frame containing 4 named components (columns):
    \itemize{
        \item \code{noten}: Integer - the notes' pitch in halftones from diapason (a),
            i.e. 0 for diapason a, 3 for c', ...
        \item \code{laenge}: Integer - denominator of lengths of the notes, e.g. 8 for a quaver.
        \item \code{punkt}: Logical - whether to punctate a note.
        \item \code{bindung}: Logical - \code{TRUE} indicates to start a slur, or to end it.
            That means that the first, third, ... occurences of \code{TRUE} start slurps,
            while the second, fourth, ... occurences end slurps.
            Note that it is only possible to draw one slur at a time.
    }
  }
  \item{file}{The file to be written for \emph{LilyPond}'s input.}
  \item{Dur}{Logical indicating major key (if \code{TRUE}) or minor key.}
  \item{grundton}{Keynote, necessary to set sharps/flats.}
  \item{schlusselart}{Integer indicating the kind of clef. The following four kinds are supported:
    1 for g-/treble-clef, 2 for f-/bass-clef, 3 for c-/alto-clef, and 4 for c-/tenor-clef.}
  \item{takt}{Character indicating which meter to use, examples are: \code{"3/4"}, \code{"4/4"}.}
  \item{endbar}{Logical indicating whether to set an ending bar at the end of the sheet music.}
  \item{midi}{Logical indicating whether Midi output (by \emph{LilyPond}) is desirable.}
  \item{tempo}{Character specifying the tempo to be used for the Midi file if \code{midi = TRUE}.
    The default, \code{"2 = 60"} indicates: 60 half notes per minute, whereas \code{"4 = 90"}
    indicates 90 quarters per minute.}
  \item{textheight}{Textheight of the sheet music te be written by \emph{LilyPond}.}
  \item{linewidth}{Linewidth of the sheet music te be written by \emph{LilyPond}.}
  \item{indent}{Indentation of the sheet music te be written by \emph{LilyPond}.}
}
\details{
    Details will be given when development has reached a stable stage ...
}
\value{
    Nothing is returned, but a \code{file} is written.
}
\references{
    Nienhuys, H.-W., Nieuwenhuizen, J. et al. (2004):
    \emph{GNU LilyPond - The Music Typesetter}.
    Free Software Foundation, \url{http://www.lilypond.org/}, Version 2.0.3.

    Preusser, A., Ligges, U. und Weihs, C. (2002): 
    \emph{Ein R Exportfilter für das Notations- und Midi-Programm LilyPond}.   
    Arbeitsbericht 35. Fachbereich Statistik, Universität Dortmund. (german)
}
\author{Andrea Preusser and Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\note{
    This function is in development! 
    Everything, in particular its user interface (and german names of arguments) is subject to change!
}
\seealso{Functions to be referenced are not yet documented ...}
\examples{
# Not yet available ...
}
\keyword{file}
\keyword{interface}
