\name{readWave}
\alias{readWave}
\title{Reading Wave files}
\description{Reading Wave files.}
\usage{
readWave(filename, from = 1, to = Inf, 
    units = c("samples", "seconds", "minutes", "hours"), header = FALSE, toWaveMC = NULL)
}
\arguments{
    \item{filename}{Filename of the file to be read.}
    \item{from}{Where to start reading (in order to save memory by reading wave file piecewise), in \code{units}.}
    \item{to}{Where to stop reading (in order to save memory by reading wave file piecewise), in \code{units}.}
    \item{units}{Units in which \code{from} and \code{to} is given, the default is \code{"samples"},
        but can be set to time intervals such as \code{"seconds"}, see the Usage Section above.}
    \item{header}{If \code{TRUE}, just header information of the Wave file are returned, 
        otherwise (the default) the whole Wave object.}
    \item{toWaveMC}{If \code{TRUE}, a \link{WaveMC-class} object is returned. 
        If \code{NULL} (default) or \code{FALSE} and a non-extensible Wave file or an extensible Wave file with no other than the \dQuote{FL} and \dQuote{FR}
            channels is found, a \link{Wave-class} object is returned, otherwise a \link{WaveMC-class} object.}
}
\value{
    An object of class \code{\link{Wave}}  or \code{\link{WaveMC}} 
        or a list containing just the header information if \code{header = TRUE}.
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}},  \code{\link{writeWave}}}
\examples{
Wobj <- sine(440)

tdir <- tempdir()
tfile <- file.path(tdir, "myWave.wav")
writeWave(Wobj, filename = tfile)
list.files(tdir, pattern = "\\\\.wav$")
newWobj <- readWave(tfile)
newWobj
file.remove(tfile)
}
\keyword{IO}
\keyword{file}
\concept{Wave}
\concept{file}
\concept{sound}
\concept{music}
\concept{speech}
