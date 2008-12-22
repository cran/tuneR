\name{channel}
\alias{channel}
\title{Channel conversion for Wave objects}
\description{Convenient wrapper to extract one or more channels (or mirror channels) from an object of class \code{Wave}.}
\usage{
channel(object, which = c("both", "left", "right", "mirror"))
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{which}{Character indicating which channel(s) should be returned.}
}
\value{\code{Wave} object including channels specified by \code{which}.}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\seealso{\code{\link{Wave}}, \code{\link{mono}}, \code{\link{extractWave}}}
\keyword{manip}
\keyword{utilities}
\concept{Wave}
\concept{channel}
