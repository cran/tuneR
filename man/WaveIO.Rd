\name{WaveIO}
\alias{readWave}
\alias{writeWave}
\title{Reading and writing Wave files}
\description{Reading and writing Wave files.}
\usage{
readWave(filename, from = 1, to = Inf, 
    units = c("samples", "seconds", "minutes", "hours"), header = FALSE)
writeWave(object, filename)
}
\arguments{
    \item{filename}{Filename of the file to be read or written.}
    \item{from}{where to start reading (in order to save memory by reading wave file piecewise), in \code{units}.}
    \item{to}{where to stop reading (in order to save memory by reading wave file piecewise), in \code{units}.}
    \item{units}{units in which \code{from} and \code{to} is given, the default is \dQuote{samples},
        but can be set to time intervals such as \dQuote{seconds}, see the Usage Section above.}
    \item{header}{if \code{TRUE}, just header information of the Wave file are returned, 
        otherwise (the default) the whole Wave object.}
    \item{object}{Object of class \code{\link{Wave}} to be written to a Wave file.}
}
\value{
    \code{readWave} returns an object of class \code{\link{Wave}} 
        or a list containing just the header information if \code{header = TRUE}.

    \code{writeWave} creates a Wave file, but returns nothing.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\code{\link{Wave}}, \code{\link{normalize}}}
\examples{
Wobj <- sine(440, bit = 16)

tdir <- tempdir()
tfile <- file.path(tdir, "myWave.wav")
writeWave(Wobj, filename = tfile)
list.files(tdir, pattern = "\\.wav$")
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
