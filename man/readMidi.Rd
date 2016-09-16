\name{readMidi}
\alias{readMidi}
\title{Read a MIDI file}
\description{A MIDI file is read and returned in form of a structured data frame containing 
most event information (minus some meta events and minus all system events). 
For details about the represented information see the reference given below.
}
\usage{
readMidi(file)
}
\arguments{
  \item{file}{Filename of MIDI file.}
}
\value{
A data frame consisting of columns
\item{time}{Time or delta-time of the events, depending on the MIDI format.}
\item{event}{A factor indicating the event.}       
\item{type}{An integer indicating the type of a \dQuote{meta event}, otherwise \code{NA}.}
\item{channel}{The channel number or \code{NA} if not applicable.}
\item{parameter1}{First parameter of an event, e.g. a representation for a note in a \dQuote{note event}.}
\item{parameter2}{Second parameter of an event.}
\item{parameterMetaSystem}{Information in a \dQuote{meta event}, currently all meta events are converted to a character representation (of hex, if all fails), but future versions may have more appropriate representations.}
\item{track}{The track number.}

Please see the given reference about the MIDI file format about details.
}
\references{A good reference about the Midi file format can be found at \url{http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html}.}
\author{Uwe Ligges and Johanna Mielke}
\note{The data structure may be changed or extended in future versions.}
\seealso{
The function \code{\link{getMidiNotes}} extracts a more readable representation of note events only.

You may also want to read Wave (\code{\link{readWave}}) or MP3 (\code{\link{readMP3}}).
}
\examples{
content <- readMidi(system.file("example_files", "Bass_sample.mid", package="tuneR"))
str(content)
content
}
\keyword{IO}
\keyword{file}
\concept{MIDI}
\concept{file}

