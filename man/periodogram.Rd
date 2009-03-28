\name{periodogram-methods}
\docType{methods}
\alias{periodogram}
\alias{periodogram-methods}
\alias{periodogram,Wave-method}
\alias{periodogram,character-method}
\title{Periodogram (Spectral Density) Estimation on Wave objects}
\description{This function estimates one or more periodograms (spectral densities) 
    of the time series contained in an object of class \code{Wave} (or directly in a Wave file)
    using a window running through the time series (possibly with overlapping).
    It returns an object of class \code{Wspec}.}
\usage{
periodogram(object, ...)
\S4method{periodogram}{Wave}(object, width = length(object), overlap = 0,
    starts = NULL, ends = NULL, taper = 0, normalize = TRUE, 
    frqRange = c(-Inf, Inf), ...)
\S4method{periodogram}{character}(object, width, overlap = 0, from = 1, to = Inf, 
    units = c("samples", "seconds", "minutes", "hours"), 
    downsample = NA, channel = c("left", "right"), pieces = 1, ...)
}
\arguments{
  \item{object}{An object of class \code{\link{Wave}} or a character string pointing to a Wave file.}
  \item{width}{A window of width \sQuote{\code{width}} running through the time series selects the samples 
    from which the periodograms are to be calculated.}
  \item{overlap}{The window can be applied by each overlapping \code{overlap} samples.}
  \item{starts}{Start number (in samples) for a window.
    If not given, this value is derived from argument \code{ends},
    or will be derived \code{width} and \code{overlap}.}
  \item{ends}{End number (in samples) for a window.
    If not given, this value is derived from argument \code{starts},
    or will be derived from \code{width} and \code{overlap}.}
  \item{taper}{proportion of data to taper. See \code{\link{spec.pgram}} for details.}
  \item{normalize}{Logical; if \code{TRUE} (default), two steps will be applied:
    (i) the input signal will be normalized to amplitude \code{max(abs(amplitude)) == 1},
    (ii) the resulting \code{spec} values will be normalized to sum up to one for each periodogram.}
  \item{frqRange}{Numeric vector of two elements indicating minimum and maximum of the frequency range 
    that is to be stored in the resulting object. This is useful to reduce memory consumption.}
  \item{from}{Where to start reading in the Wave file, in \code{units}.}
  \item{to}{Where to stop reading in the Wave file, in \code{units}.}
  \item{units}{Units in which \code{from} and \code{to} is given, the default is \dQuote{samples},
        but can be set to time intervals such as \dQuote{seconds}, see the Usage Section above.}
  \item{downsample}{Sampling rate the object is to be downsampled to.
        If \code{NA}, the default, no changes are applied. Otherwise \code{downsample} must be in \code{[2000, 192000]}; 
        typical values are 11025, 22050, and 44100 for CD quality. See also \code{\link{downsample}}.}
  \item{channel}{Character, indicating whether the \dQuote{left} or \dQuote{right} channel should be extracted 
        (see \code{\link{mono}} for details) - stereo processing is not yet implemented.}
  \item{pieces}{The Wave file will be read in in \code{pieces} steps in order to reduce the amount 
        of required memory.}
  \item{\dots}{Further arguments to be passed to the underlying function \code{\link{spec.pgram}}.}
}
\value{
An object of class \code{\link{Wspec}} is returned containing the following slots.
    \item{freq}{Vector of frequencies at which the spectral density is estimated. 
        See \code{\link{spectrum}} for details. (1)}
    \item{spec}{List of vectors or matrices of the \code{spec} values returned by \code{\link{spec.pgram}}
        at frequencies corresponding to \code{freq}.
        Each element of the list corresponds to one periodogram estimated from samples of the window
        beginning at \code{start} of the \code{\link{Wave}} object.}
    \item{kernel}{The kernel argument, or the kernel constructed from spans returned by \code{\link{spec.pgram}}. (1)}
    \item{df}{The distribution of the spectral density estimate can be approximated by a chi square distribution with 
        \code{df} degrees of freedom. (1)}
    \item{taper}{The value of the \code{taper} argument. (1)}
    \item{width}{The value of the \code{width} argument. (1)}
    \item{overlap}{The value of the \code{overlap} argument. (1)}
    \item{normalize}{The value of the \code{normalize} argument. (1)}
    \item{starts}{If the argument \code{starts} was given in the call, its value.
        If the argument \code{ends} was given in the call, \sQuote{\code{ends - width}}.
        If neither \code{starts} nor \code{ends} was given, the start points of all periodograms.
        In the latter case the start points are calculated from the arguments \code{width} and \code{overlap}.}
    \item{stereo}{Whether the underlying \code{\link{Wave}} \code{object} was stereo ot not. (1)}
    \item{samp.rate}{Sampling rate of the underlying \code{\link{Wave}} \code{object}. (1)}
    \item{variance}{The variance of samples in each window, corresponding to amplitude / loudness of sound.}
    \item{energy}{The \dQuote{energy} \eqn{E}{E}, also an indicator for the amplitude / loudness of sound:
       \deqn{E(x_I) := 20 * log_{10} \sum_{j\in I}|x_j|,}{%
             E(x_I) := 20 * log_{10} sum_{j in I} |x_j|,}
       where \eqn{I}{I} indicates the interval \eqn{I:=}{I:=} \code{start[i]:end[i]} for all 
       \eqn{i:=1,\dots,}{1:=1,...,} \code{length(starts)}.
    }
Those slots marked with \dQuote{(1)} contain the information once, 
because it is unique for all periodograms of estimated by the function call.
}
\author{Uwe Ligges, \email{ligges@statistik.tu-dortmund.de}}
\note{
Support for processing of stereo \code{\link{Wave}} objects has not yet been implemented.
}
\seealso{\itemize{
    \item for the resulting objects' class: \code{\link{Wspec}},
    \item for plotting: \code{\link{plot-Wspec}},  
    \item for the underlying periodogram calculations: \code{\link{spec.pgram}},
    \item for the input data class: \link{Wave-class}, \code{\link{Wave}}.
}}
\examples{
# constructing a Wave object (1 sec.) containing sinus sound with 440Hz:
Wobj <- sine(440, bit = 16)
Wobj

# Calculate periodograms in windows of 4096 samples each - without
#   any overlap - resulting in an Wspec object that is printed:
Wspecobj <- periodogram(Wobj, width = 4096)
Wspecobj

# Plot the first periodogram from Wspecobj:
plot(Wspecobj)
# Plot the third one and choose a reasonable xlim:
plot(Wspecobj, which = 3, xlim = c(0, 1000))
# Mark frequency that has been generated before:
abline(v = 440, col="red")

FF(Wspecobj)              # all ~ 440 Hertz
noteFromFF(FF(Wspecobj))  # all diapason a
}
\keyword{ts}
\concept{Wave}
\concept{spectrum}
