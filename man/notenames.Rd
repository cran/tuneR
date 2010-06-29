\name{notenames}
\alias{notenames}
\title{Generating note names from numbers}
\description{A function that generates note names from numbers}
\usage{
notenames(notes, language = c("english", "german"))
}
\arguments{
\item{notes}{An interger values vector, where 0 corresponds to \kbd{a'}, 
    notes below and above have to be specified in the corresponding halftone distance.}
\item{language}{Language of the note names. Currently only english and german are supported.}
}
\value{
A character vector of note names.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\examples{
notenames(c(-24, -12, 0, 12)) # octaves of a
notenames(3:15)               # chromaticism

## same in german:
notenames(3:15, language = "german")
}
\keyword{utilities}
\concept{note}

