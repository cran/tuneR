\name{play-methods}
\docType{methods}
\alias{play}
\alias{play,character-method}
\alias{play,WaveGeneral-method}
\title{Playing Waves}
\description{Plays wave files and objects of class \code{Wave}.}
\usage{
play(object, player, ...)
}
\arguments{
    \item{object}{Either a filename pointing to a Wave file, 
        or an object of class \code{\link{Wave}} or \code{\link{WaveMC}}.
        If the latter, it is written to a temporary file by \code{\link{writeWave}},
        played by the chosen player, and deleted afterwards.}
    \item{player}{(Path to) a program capable of playing a wave file by invocation from the command line.
        If under Windows and no player is given, \dQuote{mplay32.exe} or \dQuote{wmplayer.exe} 
        (if the former does not exists as under Windows 7) will be chosen as the default.}
    \item{\dots}{Further arguments passed to the Wave file \code{player}.
        If no \code{player} and no further arguments are given under Windows, the default is: \code{"/play /close"}.}
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}}
\seealso{\link{Wave-class}, \link{WaveMC-class}, \code{\link{Wave}}, \code{\link{WaveMC}}, \code{\link{writeWave}}, \code{\link{setWavPlayer}}}
\keyword{utilities}
\keyword{IO}
\keyword{interface}
\keyword{methods}
\concept{Wave}
\concept{player}
\concept{sound}
\concept{music}
\concept{speech}
