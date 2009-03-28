\name{play-methods}
\docType{methods}
\alias{play-methods}
\alias{play}
\alias{play,character-method}
\alias{play,Wave-method}
\title{Playing Waves}
\description{Plays wave files and objects of class Wave.}
\usage{
play(object, player, ...)
}
\arguments{
    \item{object}{Either a filename pointing to a Wave file, 
        or an object of class \code{\link{Wave}}.
        If the latter, it is written to a temporary file by \code{\link{writeWave}},
        played by the chosen player, and deleted afterwards.}
    \item{player}{(Path to) a program capable of playing a wave file by invocation from the command line.
        If under Windows and no player is given, \dQuote{mplay32.exe} will be chosen as the default.}
    \item{\dots}{Further arguments passed to the Wave file \code{player}.
        If no \code{player} and no further arguments are given under Windows, the default is: \code{"/play /close"}.}
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \code{\link{writeWave}}, \code{\link{setWavPlayer}}}
\keyword{utilities}
\keyword{IO}
\keyword{interface}
\keyword{methods}
\concept{Wave}
\concept{player}
\concept{sound}
\concept{music}
\concept{speech}
