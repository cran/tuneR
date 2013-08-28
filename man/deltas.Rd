\name{deltas}
\alias{deltas}
\title{ Calculate delta features }
\description{
Calculate the deltas (derivatives) of a sequence of features using a w-point
window with a simple linear slope.
}
\usage{
deltas(x, w = 9)
}
\arguments{
  \item{x}{ Matrix of features. Every column represents one time frame.
    Each row is filtered separately. }
  \item{w}{ Window width (usually odd). }
}
\details{ This function mirrors the delta calculation performed in HTKs \sQuote{feacalc}.}
\value{
  Returns a matrix of the delta features (one column per frame).
}
\references{Daniel P. W. Ellis: \url{http://www.ee.columbia.edu/~dpwe/resources/matlab/rastamat/}}
\author{ Sebastian Krey \email{krey@statistik.tu-dortmund.de} }
\examples{
  testsound <- normalize(sine(400) + sine(1000) + square(250), "16")
  m <- melfcc(testsound, frames_in_rows=FALSE)
  d <- deltas(m)
}
\concept{ deltas }
