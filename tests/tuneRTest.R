library("tuneR")
library("tools")

x1 <- sine(660, pcm=TRUE, bit=8, duration=500)
x2 <- sine(440, pcm=TRUE, bit=8, duration=500)
x3 <- sine(220, pcm=TRUE, bit=8, duration=500)
x1_2 <- stereo(sine(440, bit=8, pcm=TRUE, duration=500), sine(220, bit=8, pcm=TRUE, duration=500))

## writeWave (always Wave extensible format)
files <- tempfile(as.character(1:100), fileext = ".wav")
## 8bit (pcm)
## - mono
writeWave(x2, file = files[1]) 
## - stereo
writeWave(stereo(x2, x3), file = files[2])
## 16bit (pcm)
## - mono
writeWave(sine(440, pcm=TRUE, bit=16, duration=500), file = files[3])
## - stereo
writeWave(stereo(sine(440, bit=16, pcm=TRUE, duration=500), sine(220, bit=16, pcm=TRUE, duration=500)), file = files[4])
## 24bit (pcm)
## - mono
writeWave(sine(440, pcm=TRUE, bit=24, duration=500), file = files[5])
## - stereo
writeWave(stereo(sine(440, bit=24, pcm=TRUE, duration=500), sine(220, bit=24, pcm=TRUE, duration=500)), file = files[6])
## 32bit
sine32 <- sine(440, bit=32, duration=500)
sine32@left <- round(sine32@left, 5)
## - IEEE_FLOAT mono
writeWave(sine32, file = files[7])
## - pcm mono
writeWave(sine(440, pcm=TRUE, bit=32, duration=500), file = files[8])
## - IEEE_FLOAT stereo
writeWave(stereo(sine32, sine32), file = files[9])
## - pcm stereo
writeWave(stereo(sine(440, bit=32, pcm=TRUE, duration=500), sine(220, bit=32, pcm=TRUE, duration=500)), file = files[10])
## 64bit (IEEE_FLOAT)
exact64 <- c(-2^(-40:0), 2^(-40:0))
## - mono
writeWave(Wave(left = exact64, bit = 64, pcm = FALSE, samp.rate = 44100), file = files[11])
## - stereo
writeWave(Wave(left = exact64, right = exact64, bit = 64, pcm = FALSE, samp.rate = 44100), file = files[12])
## multi channel
mc <- WaveMC(cbind(x1@left, x2@left, x3@left), bit = 16, samp.rate = 44100)
colnames(mc) <- c("FL","FR","FC")
writeWave(mc, file = files[13]) 
colnames(mc) <- c("FL","FC","FR")
writeWave(mc, file = files[14])
as.vector(md5sum(files[1:14]))

## Waveforms
## sine
(x2)
length(x2@left)
x2@stereo
x2@samp.rate
x2@bit
x2@pcm
(x4 <- stereo(x2, x3))
length(x4@left)
x4@stereo
x4@samp.rate
x4@bit
x4@pcm
(x5 <- sine(440, bit=32, duration = 500))
length(x5@left)
x5@stereo
x5@samp.rate
x5@bit
x5@pcm
(x6 <- sine(440, bit=8, pcm=TRUE, samp.rate=11025, duration=500))
length(x6@left)
x6@stereo
x6@samp.rate
x6@bit
x6@pcm 
(x7 <- sine(440, bit=8, pcm=TRUE, stereo = TRUE, duration=500))
length(x7@left)
x7@stereo
x7@samp.rate
x7@bit
x7@pcm

## normalize
(x8 <- normalize(x1, unit = "1", center = TRUE, level = 1, rescale = TRUE))
round(x8@left[1:10], 5)
(x9 <- normalize(x1, unit = "1", center = TRUE, level = 1, rescale = FALSE))
round(x9@left[1:10], 5)
(x10 <- normalize(x1, unit = "8", center = TRUE, level = 2, rescale = TRUE))
x10@left[1:10]
(x12 <- normalize(x1, unit = "8", center = TRUE, level = 2, rescale = FALSE))
x12@left[1:10]
(x13 <- normalize(x1, unit = "16", center = TRUE, level = 1, rescale = TRUE)) 
x13@left[1:10]
(x14 <- normalize(x1, unit = "16", center = TRUE, level = 1, rescale = FALSE)) 
x14@left[1:10]
(x15 <- normalize(x1, unit = "24", center = TRUE, level = 1, rescale = TRUE))
x15@left[1:10]
(x16 <- normalize(x1, unit = "24", center = TRUE, level = 1, rescale = FALSE))
x16@left[1:10]
(x17 <- normalize(x1, unit = "32", center = TRUE, level = 1, rescale = TRUE))
x17@left[1:10]
(x18 <- normalize(x1, unit = "32", center = TRUE, level = 1, rescale = FALSE))
x18@left[1:10]
(x19 <- normalize(x1, unit = "32", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
round(x19@left[1:10], 5)
(x20 <- normalize(x1, unit = "32", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
round(x20@left[1:10], 5)
(x21 <- normalize(x1, unit = "64", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
round(x21@left[1:10], 8)
(x22 <- normalize(x1, unit = "64", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
round(x22@left[1:10], 8)
(x23 <- normalize(x1_2, unit = "1", center = TRUE, level = 1, rescale = TRUE))
round(x23@left[1:10], 5)
round(x23@right[1:10], 5)
(x24 <- normalize(x1_2, unit = "1", center = TRUE, level = 1, rescale = FALSE))
round(x24@left[1:10], 5)
round(x24@right[1:10], 5)
(x25 <- normalize(x1_2, unit = "8", center = TRUE, level = 2, rescale = TRUE))
x25@left[1:10]
x25@right[1:10]
(x26 <- normalize(x1_2, unit = "8", center = TRUE, level = 2, rescale = FALSE))
x26@left[1:10]
x26@right[1:10]
(x27 <- normalize(x1_2, unit = "16", center = TRUE, level = 1, rescale = TRUE)) 
x27@left[1:10]
x27@right[1:10]
(x28 <- normalize(x1_2, unit = "16", center = TRUE, level = 1, rescale = FALSE)) 
x28@left[1:10]
x28@right[1:10]
(x29 <- normalize(x1_2, unit = "24", center = TRUE, level = 1, rescale = TRUE))
x29@left[1:10]
x29@right[1:10]
(x30 <- normalize(x1_2, unit = "24", center = TRUE, level = 1, rescale = FALSE))
x30@left[1:10]
x30@right[1:10]
(x31 <- normalize(x1_2, unit = "32", center = TRUE, level = 1, rescale = TRUE))
x31@left[1:10]
x31@right[1:10]
(x32 <- normalize(x1_2, unit = "32", center = TRUE, level = 1, rescale = FALSE))
x32@left[1:10]
x32@right[1:10]
(x33 <- normalize(x1_2, unit = "32", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
round(x33@left[1:10], 5)
round(x33@right[1:10], 5)
(x34 <- normalize(x1_2, unit = "32", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
round(x34@left[1:10], 5)
round(x34@right[1:10], 5)
(x35 <- normalize(x1_2, unit = "64", center = TRUE, level = 1, rescale = TRUE, pcm = FALSE))
round(x35@left[1:10], 8)
round(x35@right[1:10], 8)
(x36 <- normalize(x1_2, unit = "64", center = TRUE, level = 1, rescale = FALSE, pcm = FALSE))
round(x36@left[1:10], 8)
round(x36@right[1:10], 8)
