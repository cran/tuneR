\name{WavPlayer}
\alias{setWavPlayer}
\alias{getWavPlayer}
\title{Getting and setting the default player for Wave files}
\description{Getting and setting the default player for Wave files}
\usage{
setWavPlayer(player)
getWavPlayer()
}
\arguments{
  \item{player}{Set the character string to call a Wave file player (including optional arguments) 
    using \code{\link{options}}.}
}
\value{
    \code{getWavPlayer} returns the character string that has been set by \code{setWavPlayer}.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\code{\link{Wave}}, \code{\link{play}}}
\keyword{utilities}
\concept{Wave}
\concept{player}
\concept{sound}
\concept{music}
\concept{speech}
