\name{noteFromFF}
\alias{noteFromFF}
\title{Deriving notes from frequencies}
\description{Deriving notes from given (fundamental) frequencies.}
\usage{
noteFromFF(x, diapason = 440, roundshift = 0)
}
\arguments{
    \item{x}{Fundamental frequency.}
    \item{diapason}{Frequency of diapason a, default is 440 (Hertz).}
    \item{roundshift}{Shift that indicates from here to round to the next integer (note).
        The default (0) is \dQuote{classical} rounding as described in \code{\link{round}}.
        A higher value means that \code{roundshift} is added to the calculated real note value
        before rounding to an integer. This is useful if it is unclear that some instruments really 
        shift the note in the center between two theoretical frequencies.
        
        Example: if \code{x=452} and \code{diapason=440}, the internally calculated 
        real value of 0.46583 is rounded to 0,
        but for \code{roundshift=0.1} we get 0.56583 and it is rounded to note 1.}
}
\value{
An integer representing the (rounded) difference in halftones from diapason a,
    i.e. indicating the note that corresponds to fundamental frequency \code{x} given the value of \code{diapason}.
    For example: 0 indicates diapason a, 3: c', 12: a', \dots
}
\details{The formula used is simply \code{round(12 * log(x / diapason, 2) + roundshift)}.}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{FF}}, \code{\link{periodogram}}, and \link{tuneR} for a very complete example.}
\keyword{utilities}
\concept{note}
\concept{pitch}
\concept{fundamental}
\concept{frequency}
\concept{tracking}
\concept{f0}
\concept{periodogram}
\concept{Wave}
