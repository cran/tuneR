\name{getMidiNotes}
\alias{getMidiNotes}
\title{Extract note events from objects returned by readMidi}
\description{Extract only note events from an object returned by the \code{\link{readMidi}} function.}
\usage{
getMidiNotes(x, ...)
}
\arguments{
  \item{x}{A data.frame returned by the \code{\link{readMidi}} function.}
  \item{\dots}{Further arguments are passed to the \code{\link{notenames}} function for extracting 
    the human readable note names rather than their integer representations.}
}
\value{
A data frame with columns
\item{time}{start time}
\item{length}{length}
\item{track}{track number}
\item{channel}{channel number}
\item{note}{note}
\item{notename}{notename}
\item{velocity}{note velocity}
}
\author{Uwe Ligges and Johanna Mielke}
\seealso{\code{\link{readMidi}}}
\examples{
content <- readMidi(system.file("example_files", "Bass_sample.mid", package="tuneR"))
getMidiNotes(content)
}

