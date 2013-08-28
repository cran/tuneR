\name{smoother}
\alias{smoother}
\title{Meta Function for Smoothers}
\description{Apply a smoother to estimated notes.
Currently, only a running median (using \code{\link[pastecs]{decmedian}} in package \pkg{pastecs}) is available.}
\usage{
smoother(notes, method = "median", order = 4, times = 2)
}
\arguments{
\item{notes}{Series of notes, a vector of integers such as returned by \code{\link{noteFromFF}}.}
\item{method}{Currently, only a running \code{'median'} 
    (using \code{\link[pastecs]{decmedian}} in package \pkg{pastecs}) is available.}
\item{order}{The window used for the running median corresponds to 2*order + 1.}
\item{times}{The number of times the running median is applied (default: 2).}
}
\value{
The smoothed series of notes.
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\keyword{misc}
\keyword{ts}
\concept{smooth}
\concept{median}
\concept{running}
