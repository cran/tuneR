\name{tuneR}
\alias{tuneR}
\title{tuneR}
\description{tuneR, a collection of examples}
\section{Functions in tuneR}{
\emph{tuneR} consists of several functions to work with and to analyze Wave files.
In the following examples, some of the functions 
to read and write Wave files (\code{\link{readWave}}, \code{\link{writeWave}}),
to represent or construct Wave files (\code{\link{Wave}}),
to transform  Wave objects (\code{\link{bind}}, \code{\link{channel}}, 
\code{\link{downsample}}, \code{\link{extractWave}}, \code{\link{mono}}, \code{\link{stereo}}),
and to \code{\link{play}} Wave objects are used.
Other functions and classes are avaiable to 
calculate several periodograms of a signal (\code{\link{periodogram}}, \code{\link{Wspec}}),
to estimate the corresponding fundamental frequencies (\code{\link{FF}}, \code{\link{FFpure}}),
to derive the corresponding notes (\code{\link{noteFromFF}}), 
and to apply a \code{\link{smoother}}.
Of course, print (show), plot and summary methods are available for most classes.
Moreover, a function called \code{\link{lilyinput}} can prepare a data frame to be presented 
as sheet music by postprocessing with the music typesetting software LilyPond.
}
\author{Uwe Ligges, \email{ligges@statistik.uni-dortmund.de}}
\examples{
\dontrun{
# library(tuneR) # in a regular session, we are loading tuneR
  
# constructing a mono Wave object (2 sec.) containing sinus 
# sound with 440Hz and folled by 220Hz:
x <- seq(0, 2*pi, length = 44100)
channel <- round(32000 * c(sin(440 * x), sin(220 * x)))
Wobj <- Wave(left = channel)
show(Wobj)
plot(Wobj) # it does not make sense to plot the whole stuff
plot(extractWave(Wobj, from = 1, to = 500))
play(Wobj) # listen to the sound

# write the Wave object into a Wave file (can be played with any player):
writeWave(Wobj, "testfile.wav")
# reading it in again:
Wobj2 <- readWave("testfile.wav")
identical(Wobj2, Wobj) # it is still the same 

Wobjm <- mono(Wobj, "left") # extract the left channel
# and downsample to 11025 samples/sec.:
Wobjm11 <- downsample(Wobjm, 11025)
# extract a part of the signal interactively (click for left/right limits):
Wobjm11s <- extractWave(Wobjm11)

# calculating periodograms of sections each consisting of 1024 observations,
# overlapping by 512 observations:
WspecObject <- periodogram(Wobjm11s, normalize = TRUE, width = 1024, overlap = 512)
# Let's look at the first periodogram:
plot(WspecObject, xlim = c(0, 2000), which = 1)
# calculate the fundamental frequency:
ff <- FF(WspecObject)
print(ff)
# derive note from FF given diapason a'=440
notes <- noteFromFF(ff, 440)
# smooth the notes:
snotes <- smoother(notes)
# outcome should be 0 for diapason "a'" and -12 (12 halftones lower) for "a"
print(snotes) 
# plot melody and energy of the sound:
melodyplot(WspecObject, snotes)
}
}
\keyword{documentation}

