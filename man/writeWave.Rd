\name{writeWave}
\alias{writeWave}
\title{Writing Wave files}
\description{Writing Wave files.}
\usage{
writeWave(object, filename, extensible = TRUE)
}
\arguments{
    \item{object}{Object of class \code{\link{Wave}} or \code{\link{WaveMC}} to be written to a Wave file.}
    \item{filename}{Filename of the file to be written.}
    \item{extensible}{If \code{TRUE} (default), an extensible Wave format file is written. If \code{FALSE}, a non-extensible Wave file is written.}
}
\value{
    \code{writeWave} creates a Wave file, but returns nothing.
}
\details{
It is only possible to write a non-extensible Wave format file for objects of class \code{\link{Wave}} or
for objects of class \code{\link{WaveMC}} with one or two channels (mono or stereo).

If the argument \code{object} is a \link{Wave-class} object, the channels are automatically chosen to be
\dQuote{FL} (for mono) or \dQuote{FL} and \dQuote{FR} (for stereo).

The channel mask used to arrange the channel ordering in multi channel Wave files is written
according to Microsoft standards as given in the data frame \code{\link{MCnames}} containing the first 18 standard channels.
In the case of writing a multi channel Wave file, the column names of the object \code{object} (\code{colnames(object)}) must be specified and
must uniquely identify the channel ordering for WaveMC objects.
The column names of the object of class \code{\link{WaveMC}} have to be a subset of the 18 standard channels
and have to match the corresponding abbreviated names.
(See \code{\link{MCnames}} for possible channels and the abbreviated names: 
\dQuote{FL}, \dQuote{FR}, \dQuote{FC}, \dQuote{LF}, \dQuote{BL}, \dQuote{BR},
\dQuote{FLC}, \dQuote{FRC}, \dQuote{BC}, \dQuote{SL}, \dQuote{SR}, \dQuote{TC}, 
\dQuote{TFL}, \dQuote{TFC}, \dQuote{TFR}, \dQuote{TBL}, \dQuote{TBC} and \dQuote{TBR}).

The function \code{\link{normalize}} can be used to transform and rescale data to an appropriate amplitude range for
various Wave file formats (either pcm with 8-, 16-, 24- or 32-bit or IEEE_FLOAT with 32- or 64-bit).
}
\author{Uwe Ligges \email{ligges@statistik.tu-dortmund.de}, Sarah Schnackenberg}
\seealso{\link{Wave-class}, \code{\link{Wave}}, \link{WaveMC-class}, \code{\link{WaveMC}}, \code{\link{normalize}}, \code{\link{MCnames}}, \code{\link{readWave}}}
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

