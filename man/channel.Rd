\name{channel}
\alias{channel}
\title{Channel conversion for Wave objects}
\description{Convenient wrapper to extract one or more channels from an object of class \code{Wave}
    and return it in form of a data frame.}
\usage{
channel(object, which = c("both", "left", "right"))
}
\arguments{
  \item{object}{Object of class \code{\link{Wave}}.}
  \item{which}{Character indicating which channel(s) should be returned in form of a data frame.}
}
\value{Data frame containing the data of one or two channels from the corresponding \code{Wave} object.}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\seealso{\code{\link{Wave}}, \code{\link{mono}}, \code{\link{extractWave}}}
\keyword{manip}
\keyword{utilities}
