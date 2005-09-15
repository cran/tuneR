\name{FF}
\alias{FF}
\alias{FFpure}
\title{Estimation of Fundamental Frequencies from a Wspec object}
\description{Estimation of Fundamental Frequencies from an object of class \code{\link{Wspec}}.
    Additionally, some heuristics are used to distinguish silence, noise (and breathing for singers) 
    from real tones.}
\usage{
FF(object, peakheight = 0.01, silence = 0.2, minpeak = 9, diapason = 440, 
    notes = NULL, interest.frqs = seq(along = object@freq),
    search.par = c(0.8, 10, 1.3, 1.7))
    
FFpure(object, peakheight = 0.01, diapason = 440, 
    notes = NULL, interest.frqs = seq(along = object@freq),
    search.par = c(0.8, 10, 1.3, 1.7))
}

\arguments{
  \item{object}{An object of class \code{\link{Wspec}}.}
  \item{peakheight}{The peak's proportion of the maximal peak height to be 
    considered for fundamental frequency detection.
    The default (0.01) means peaks smaller than 0.02 times the maximal peak height are omitted.}
  \item{silence}{The maximum proportion of periodograms to be considered as silence or noise (such as breathing).
    The default (0.2) means that less than 20 out of 100 periodograms represent silence or noise.}
  \item{minpeak}{If more than \code{minpeak} peaks are considered for detection and passed argument \code{peakheight},
    such periodograms are detected to be silence or noise (if \code{silence > 0}).}
  \item{diapason}{Frequency of diapason a, default is 440 (Hertz).}
  \item{notes}{Optional, a vector of integers indicating the notes (in halftones from diapason a)
    that are expected. By applying this restriction, the \dQuote{detection error} might be reduced in some cases.}
  \item{interest.frqs}{Optional, either a vector of integers indicating the 
      indices of (fundamental) frequencies in \code{object} that are expected,
      or one of the character strings \code{"bass"}, \code{"tenor"}, \code{"alto"} or \code{"soprano"}.
      For these voice types, only typical frequency ranges are considered for detection.
        
      By applying this restriction, the \dQuote{detection error} might be reduced in some cases.}
  \item{search.par}{Parameters to look for peaks:
    \enumerate{
        \item The first peak larger than \code{peakheight * 'largest_peak'} is taken.
        \item Its frequency is multiplied by \code{1+search.par[1]}
            Now, any larger peak between the old peak and that value is taken,
            if (a) it exists and if (b) it is above the \code{search.par[2]}-th Fourier-Frequency.
        \item Within the interval of frequencies \code{'current peak' * search.par[3:4]},
            another high peak is looked for. If any high peak exists in that interval, 
            it can be assumed we got the wrong partial and the \sQuote{real} fundamental frequency can be re-estimated 
            from the next two partials.}}
}
\details{\code{FFpure} just estimates the fundamental frequencies for all periodograms contained in the 
    \code{object} (of class \code{\link{Wspec}}.
    
    \code{FF} additionally uses some heuristics to distinguish silence, noise (and breathing for singers) 
    from real tones. It is recommended to use the wrapper function \code{FF} rather than \code{FFpure}.
    If silence detecion can be omitted by specifying \code{silence = 0}.
}
\value{
   Vector of estimated fundamental frequencies (in Hertz) for each periodogram conatined in \code{object}.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\note{These functions are still in development and may be changed in due course.}
\seealso{\code{\link{Wspec}}, \code{\link{periodogram}} (including an example), \code{\link{noteFromFF}},
    and \link{tuneR} for a very complete example.}
\keyword{ts}
\concept{pitch}
\concept{fundamental}
\concept{frequency}
\concept{tracking}
\concept{f0}
\concept{periodogram}
\concept{Wave}
