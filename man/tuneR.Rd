\name{tuneR}
\alias{tuneR}
\alias{tuneR-package}
\docType{package}
\title{tuneR}
\description{tuneR, a collection of examples}
\section{Functions in tuneR}{
\emph{tuneR} consists of several functions to work with and to analyze Wave files.
In the following examples, some of the functions 
to generate some data (such as \code{\link{sine}}),
to read and write Wave files (\code{\link{readWave}}, \code{\link{writeWave}}),
to represent or construct (multi channel) Wave files (\code{\link{Wave}}, \code{\link{WaveMC}}),
to transform  Wave objects (\code{\link{bind}}, \code{\link{channel}}, 
\code{\link{downsample}}, \code{\link{extractWave}}, \code{\link{mono}}, \code{\link{stereo}}),
and to \code{\link{play}} Wave objects are used.

Other functions and classes are available to 
calculate several periodograms of a signal (\code{\link{periodogram}}, \code{\link{Wspec}}),
to estimate the corresponding fundamental frequencies (\code{\link{FF}}, \code{\link{FFpure}}),
to derive the corresponding notes (\code{\link{noteFromFF}}), 
and to apply a \code{\link{smoother}}.
Now, the melody and corresponding energy values can be plotted using the function 
\code{\link{melodyplot}}.

A next step is the quantization (\code{\link{quantize}}) and a corresponding plot 
(\code{\link{quantplot}}) showing the note values for binned data.
Moreover, a function called \code{\link{lilyinput}} 
(and a data-preprocessing function \code{\link{quantMerge}})
can prepare a data frame to be presented as sheet music by 
postprocessing with the music typesetting software LilyPond.

Of course, print (show), plot and summary methods are available for most classes.
}
\author{Uwe Ligges <ligges@statistik.tu-dortmund.de> with contributions
    from Sebastian Krey, Olaf Mersmann, Sarah Schnackenberg,
    Andrea Preusser, Anita Thieler, and Claus Weihs, as well as code fragments
    and ideas from the former package \pkg{sound} by Matthias Heymann
    and functions from \sQuote{rastamat} by Daniel P. W. Ellis.
    The included parts of the libmad MPEG audio decoder library are
    authored by Underbit Technologies.}
\examples{
library("tuneR") # in a regular session, we are loading tuneR
  
# constructing a mono Wave object (2 sec.) containing sinus 
# sound with 440Hz and folled by 220Hz:
Wobj <- bind(sine(440), sine(220))
show(Wobj)
plot(Wobj) # it does not make sense to plot the whole stuff
plot(extractWave(Wobj, from = 1, to = 500))
\dontrun{
play(Wobj) # listen to the sound
}

tmpfile <- file.path(tempdir(), "testfile.wav")
# write the Wave object into a Wave file (can be played with any player):
writeWave(Wobj, tmpfile)
# reading it in again:
Wobj2 <- readWave(tmpfile)

Wobjm <- mono(Wobj, "left") # extract the left channel
# and downsample to 11025 samples/sec.:
Wobjm11 <- downsample(Wobjm, 11025)
# extract a part of the signal interactively (click for left/right limits):
\dontrun{
Wobjm11s <- extractWave(Wobjm11)
}
# or extract some values reproducibly 
Wobjm11s <- extractWave(Wobjm11, from=1000, to=17000)

# calculating periodograms of sections each consisting of 1024 observations,
# overlapping by 512 observations:
WspecObject <- periodogram(Wobjm11s, normalize = TRUE, width = 1024, overlap = 512)
# Let's look at the first periodogram:
plot(WspecObject, xlim = c(0, 2000), which = 1)
# or a spectrogram
image(WspecObject, ylim = c(0, 1000))
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

# apply some quantization (into 8 parts): 
qnotes <- quantize(snotes, WspecObject@energy, parts = 8) 
# an plot it, 4 parts a bar (including expected values):
quantplot(qnotes, expected = rep(c(0, -12), each = 4), bars = 2)
# now prepare for LilyPond
qlily <- quantMerge(snotes, 4, 4, 2)
qlily
}
\keyword{documentation}
\concept{Wave}
\concept{periodogram}
\concept{music}
