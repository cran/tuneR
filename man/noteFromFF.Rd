\name{noteFromFF}
\alias{noteFromFF}
\title{Deriving notes from frequencies}
\description{Deriving notes from given (fundamental) frequencies.}
\usage{
noteFromFF(x, diapason = 440)
}
\arguments{
    \item{x}{Fundamental frequency.}
    \item{diapason}{Frequency of diapason a, default is 440 (Hertz).}
}
\value{
An integer representing the (rounded) difference in halftones from diapason a,
    i.e. indicating the note that corresponds to fundamental frequency \code{x} given the value of \code{diapason}.
    For example: 0 indicates diapason a, 3: c', 12: a', \dots
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{FF}}, and \code{\link{periodogram}} for an example}
\keyword{utilities}

