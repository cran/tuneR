\name{WaveIO}
\alias{readWave}
\alias{writeWave}
\title{Reading and writing Wave files}
\description{Reading and writing Wave files.}
\usage{
readWave(filename)
writeWave(object, filename)
}
\arguments{
    \item{filename}{Filename of the file to be read or written.}
    \item{object}{Object of class \code{\link{Wave}} to be written to a Wave file.}
}
\value{
    \code{readWave} returns an object of class \code{\link{Wave}}.

    \code{writeWave} creates a Wave file, but returns nothing.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
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
