\name{readMP3}
\alias{readMP3}
\title{
Read an MPEG-2 layer 3 file into a Wave object
}
\description{
  A bare bones MPEG-2 layer 3 (MP3) file reader that returns the results
  as 16bit PCM data stored in a Wave object.
}
\usage{
readMP3(filename)
}
\arguments{
  \item{filename}{Filename of MP3 file.}
}
\note{
  The decoder can currently only handle files which are either mono or
  stereo. This is a limitation of the Wave object and the underlying MAD
  decoder.  
}
\value{
  A \code{\linkS4class{Wave}} object.
}
\seealso{\code{\linkS4class{Wave}}}
\author{Olaf Mersmann \email{olafm@statistik.tu-dortmund.de}}
\references{
  The decoder source code is taken from the MAD library, see
  \url{http://www.underbit.com/products/mad/}.
}
\keyword{IO}
\keyword{file}
\concept{MP3}
\concept{file}
\concept{sound}
\concept{music}
\concept{speech}
\examples{
\dontrun{
## Requires an mp3 file named sample.mp3 in the current directory.
mpt <- readMP3("sample.mp3")
summary(mpt)
}
}
