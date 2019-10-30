## ----install, eval = FALSE-----------------------------------------------
#  install.packages("RHRV", dependencies = TRUE)

## ----installDownloaded, eval = FALSE-------------------------------------
#  setwd(sourceDirectory)
#  install.packages("RHRV_XXX",repos = NULL)

## ----library, message=FALSE, warning=FALSE-------------------------------
library('RHRV')

## ----accessingData, eval=FALSE-------------------------------------------
#  # HRVData structure containing the heart beats
#  data("HRVData")
#  # HRVData structure storing the results of processing the
#  # heart beats: the beats have been filtered, interpolated, ...
#  data("HRVProcessedData")

## ----includeFigure, echo=FALSE, fig.align='center',fig.cap="The most important fields stored in the *HRVData* structure"----
knitr::include_graphics("figures/basicHRVData.png")

## ----creation, eval=TRUE-------------------------------------------------
hrv.data  = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE )

## ----loading-------------------------------------------------------------
hrv.data = LoadBeatAscii(hrv.data, "example.beats",
                         RecordPath = "beatsFolder")

## ----derivating----------------------------------------------------------
hrv.data = BuildNIHR(hrv.data)

## ----filtering-----------------------------------------------------------
hrv.data = FilterNIHR(hrv.data)

## ----interpolating-------------------------------------------------------
# Note that it is not necessary to specify freqhr since it matches with
# the default value: 4 Hz
hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)

## ----plottingHR, fig.align="center",fig.width=5,fig.height=4-------------
PlotNIHR(hrv.data, main = "niHR")

## ----timeAnalysis, eval=FALSE--------------------------------------------
#  hrv.data = CreateTimeAnalysis(hrv.data, size = 300,
#          interval = 7.8125)

## ----completeTimeAnalysis------------------------------------------------
hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data,FALSE)
hrv.data = LoadBeatAscii(hrv.data,"example.beats","beatsFolder")
hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = SetVerbose(hrv.data,TRUE)
hrv.data = CreateTimeAnalysis(hrv.data,size=300,interval = 7.8125)
# We can access "raw" data... let's print separately, the SDNN 
# parameter
cat("The SDNN has a value of ",hrv.data$TimeAnalysis[[1]]$SDNN," msec.\n")

## ----creatingFreq--------------------------------------------------------
hrv.data = CreateFreqAnalysis(hrv.data)

## ----STFTanalysis--------------------------------------------------------
hrv.data = CreateHRVData( )
hrv.data = SetVerbose(hrv.data,FALSE)
hrv.data = LoadBeatAscii(hrv.data,"example.beats","beatsFolder")
hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = InterpolateNIHR (hrv.data, freqhr = 4)
hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = SetVerbose(hrv.data,TRUE)
# Note that it is not necessary to write the boundaries 
# for the frequency bands, since they match
# the default values
hrv.data = 
  CalculatePowerBand(hrv.data , indexFreqAnalysis = 1,
                     size = 300, shift = 30, type = "fourier",
                     ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                     LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )

## ----STFTanalysis2, eval= FALSE------------------------------------------
#  hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis= 1,
#                                size = 300, shift = 30)

## ----waveletAnalysis-----------------------------------------------------
hrv.data = CreateHRVData( )
hrv.data = SetVerbose(hrv.data,FALSE)
hrv.data = LoadBeatAscii(hrv.data,"example.beats","beatsFolder")
hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = InterpolateNIHR (hrv.data, freqhr = 4)
hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = SetVerbose(hrv.data,TRUE)
# Note that it is not necessary to write the boundaries
# for the frequency bands, since they match the default values
hrv.data =
  CalculatePowerBand( hrv.data , indexFreqAnalysis = 1,
                      type = "wavelet", wavelet = "la8",
                      bandtolerance = 0.01, relative = FALSE,
                      ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                      LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4 )

## ----echo=FALSE----------------------------------------------------------
hrv.data = CreateHRVData( )
hrv.data = SetVerbose(hrv.data, FALSE)
hrv.data = LoadBeatAscii(hrv.data,"example.beats","beatsFolder")
hrv.data = BuildNIHR(hrv.data)
hrv.data = FilterNIHR(hrv.data)
hrv.data = InterpolateNIHR(hrv.data, freqhr = 4)

## ----bothAnalysis,message=FALSE------------------------------------------
# ...
# create structure, load beats, filter and interpolate
hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = SetVerbose(hrv.data, FALSE)
# use freqAnalysis number 1 for perfoming 
# Fourier analysis. This time, we do not
# write the band's boundaries
hrv.data = CalculatePowerBand(hrv.data , indexFreqAnalysis = 1,
                              size = 300, shift = 30, sizesp = 2048, 
                              type = "fourier")
# use freqAnalysis number 2 for perfoming 
# wavelet analysis. Note the indexFreqAnalysis = 2!!!
hrv.data = CreateFreqAnalysis(hrv.data)
hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis= 2,
                              type = "wavelet", wavelet = "la8",
                              bandtolerance = 0.01, relative = FALSE)

## ----plottingFreqFourier,fig.align="center", fig.width=6,fig.height=6----
# Plotting Fourier analysis
PlotPowerBand(hrv.data, indexFreqAnalysis = 1, ymax = 200, ymaxratio = 1.7)

## ----plottingFreqWavelet,fig.align="center", fig.width=6, fig.height=6----
# Plotting wavelet analysis
PlotPowerBand(hrv.data, indexFreqAnalysis = 2, ymax = 700, ymaxratio = 50)

