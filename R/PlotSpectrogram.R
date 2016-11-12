#' PlotSpectrogram
#' @title Calculates and Plots spectrogram 
#' @description  Plots spectrogram of the heart rate signal as calculated by
#' CalculateSpectrogram() function
#' @param HRVData Data structure that stores the beats register and information
#' related to it 
#' @param size Size of window for calculating spectrogram (seconds) 
#' @param shift Displacement of window for calculating spectrogram (seconds) 
#' @param sizesp Points for calculation (zero padding). If the user does not 
#' specify it, the function estimates a propper value.
#' @param freqRange Vector with two components specifying the frequency range 
#' that the program should plot. If the user does not specify it, the function 
#' uses the whole frequency range. It is possible to specify the frequency range
#' using the ylim parameter.
#' @param scale Scale used to plot spectrogram, linear or logarithmic 
#' @param verbose  Deprecated argument maintained for compatibility, 
#' use SetVerbose() instead 
#' @param showLegend Logical argument. If true, a legend of the color map is 
#' shown (default is TRUE)
#' @param Tags List of tags to specify which episodes, as apnoea or oxygen 
#' desaturation, are included in the plot. \emph{Tags}="all" plots all episodes present
#' in the data.
#' @param Indexes List of indexes of episodes (see ListEpisodes())
#' to specify which episodes are included in the plot.
#' \emph{Indexes}="all" plots all episodes present in the data.
#' @param eplim Two-component vector specifying the y-range (min,max) for the
#' vertical lines limiting each episode.
#' @param epColorPalette Vector specifying the color of each of the episodes that 
#' will be plotted. The length of colorPalette should be equal or greater than 
#' the number of different episodes to be plotted.
#' @param markEpisodes Boolean specyfing if a horizontal mark should be included 
#' for each of the episodes.
#' @param ymark Two-component vector specifying the y-range (min,max) for the
#'  horizontal marks. Only used if markEpisodes = TRUE.
#' @param showEpLegend Boolean argument. If TRUE, a legend of the episodes is 
#' included.
#' @param epLegendCoords Two-component vector specifiying the coordinates where
#' the legend should be placed. By defaul, the legend is placed on top of the
#' plot.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis
#' @param ylim Numeric vectors of length 2, giving the x and y coordinates 
#' range. If freqRange is specified, ylim is overwriten by it because of backward
#' compatibility.
#' @param Tag Deprecated argument maintained for
#' compatibility, use Tags instead.
#' @param ... Other graphical parameters. See 
#' \code{\link[graphics]{filled.contour}}.
#' @note PlotSpectrogram with \emph{showLegend = TRUE} uses the layout function 
#' and so is restricted to a full page display. Select \emph{showLegend = FALSE}
#' in order to use the layout function.
#' @references  L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, 
#' I. Gomez-Conde, "An open source tool for heart rate variability spectral 
#' analysis", Computer Methods and Programs in Biomedicine 103, 39-50, 
#' doi:10.1016/j.cmpb.2010.05.012 (2011)
#' @author M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila. C.A. Garcia
#' @seealso \code{\link{CalculateSpectrogram}} for spectrogram calculation
#' @examples
#' \dontrun{
#' 
#' # Read file "a03" from the physionet apnea-ecg database
#' library(RHRV)
#' HRVData <- CreateHRVData()
#' HRVData <- LoadBeatWFDB(HRVData,RecordName="test_files/WFDB/a03")
#' HRVData <- LoadApneaWFDB(HRVData,RecordName="test_files/WFDB/a03")
#' # Add other type of episode for a more complete example (this episode does
#' # not have any physiological meaning)
#' HRVData <- AddEpisodes(HRVData,InitTimes=c(4500),Durations=c(1000), 
#'                        Tags="Other", Values = 1)         
#' # Calculating heart rate signal:
#' HRVData <- BuildNIHR(HRVData)
#'  
#' # Filtering heart rate signal:
#' HRVData <- FilterNIHR(HRVData)
#' 
#' # Interpolating heart rate signal:
#' HRVData = InterpolateNIHR(HRVData)
#'  
#' # Calculating and Plotting Spectrogram
#' spctr <- PlotSpectrogram(HRVData, size = 120, shift = 10, sizesp = 1024,
#'          freqRange=c(0,0.14), color.palette = topo.colors)
#'          
#' spctr <- PlotSpectrogram(HRVData,size=120, shift=60, Tags="all", 
#'                          ylim=c(0,0.1),
#'                          showLegend=T, 
#'                          eplim = c(0,0.06),
#'                          epColorPalette=c("skyblue","white"), 
#'                          showEpLegend = T,
#'                          epLegendCoords = c(15000,0.08), 
#'                          ymark=c(0.001,0.002))
#'}
#'@keywords hplot 
PlotSpectrogram <- function(HRVData, size, shift, sizesp = NULL,
                            freqRange = NULL, scale = "linear", verbose = NULL,
                            showLegend = TRUE, 
                            Tags = NULL, Indexes = NULL,
                            eplim =NULL,  epColorPalette = NULL,
                            markEpisodes = TRUE, ymark = NULL,
                            showEpLegend = TRUE, epLegendCoords = NULL,                            
                            main="Spectrogram of the HR  series",
                            xlab = "Time (sec.)", ylab="Frequency (Hz.)",
                            ylim = freqRange, Tag = NULL,                           
                            ...){
  # -----------------
  # Plots spectrogram
  # -----------------
  #    size, disp: size and displacement of window (sec.)
  #    sizesp: seconds for calculating spectrogram (zero padding)
  #	   scale: linear or logarithmic
  
  HRVData = HandleVerboseArgument(HRVData, verbose)
  
  Tags = HandleDeprecatedTagArgument(Tag = Tag, Tags = Tags)
  
  VerboseMessage(HRVData$Verbose, "Plotting spectrogram")
  
  specgr=CalculateSpectrogram(HRVData,size,shift,sizesp)
  
  if(scale=="logarithmic"){
    specgr=log(specgr)
  }
  
  frequency = seq(from=0,to=HRVData$Freq_HR/2,length.out=ncol(specgr))
  time = seq(from=head(HRVData$Beat$Time,1),to=tail(HRVData$Beat$Time,1),length.out = nrow(specgr))
  
  if (is.null(freqRange)){
    if (is.null(ylim)){
      freqRange = range(frequency)  
      ylim = freqRange
    }else{
      freqRange = ylim
    }    
  }else{
    ylim = freqRange
  }
  indx = which(frequency >= freqRange[[1]] & frequency <= freqRange[[2]])

  par.orig <- par(c("mar", "las", "mfrow"))
  on.exit(par(par.orig))

  
  rhrv.filled.contour(time, frequency[indx], specgr[,indx], show.legend=showLegend,xlab = xlab, ylab = ylab, main =main, ...)  
  
  if (!is.null(Tags) || !is.null(Indexes)){
    
    if (is.null(eplim)) eplim = c(ylim[1], 0.90 * ylim[2])
    if (is.null(ymark)) ymark = c(0.99 * eplim[2], eplim[2])
    OverplotEpisodes(HRVData, Tags=Tags, Indexes=Indexes, eplim = eplim,
                     showEpLegend = showEpLegend,                     
                     epLegendCoords = epLegendCoords,
                     epColorPalette = epColorPalette,
                     markEpisodes = markEpisodes,
                     ymark = ymark)
  }
  
  VerboseMessage(HRVData$Verbose, "Spectrogram plotted")
	
  return(specgr)
}

