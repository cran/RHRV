#' 
#' OverplotEpisodes
#' @description Add episodic information to the current plot
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param Tags List of tags to specify which episodes, as apnoea or oxygen 
#' desaturation, are included in the plot. \emph{Tags}="all" plots all episodes present
#' in the data.
#' @param Indexes List of indexes of episodes (see \code{\link{ListEpisodes}})
#' to specify which episodes are included in the plot. 
#' \emph{Indexes}="all" plots all episodes present in the data.
#' @param epColorPalette Vector specifying the color of each of the episodes that
#' will be plotted. The length of epColorPalette should be equal or greater than
#' the number of different episodes to be plotted.
#' @param eplim Two-component vector specifying the y-range (min,max) for the 
#' vertical lines limiting each episode.
#' @param lty The line type for the vertical lines limiting each episode.
#' @param markEpisodes Boolean specyfing if a horizontal mark should be included
#' for each of the episodes.
#' @param ymark Two-component vector specifying the y-range (min,max) for
#' the horizontal marks. Only used if markEpisodes = TRUE.
#' @param showEpLegend Boolean argument. If TRUE, a legend of the episodes is 
#' included.
#' @param epLegendCoords Two-component vector specifiying the coordinates where
#' the legend should be placed. By defaul, the legend is placed on top of 
#' the plot. 
#' @param Tag Deprecated argument maintained for
#' compatibility, use Tags instead.
#' @param ... Other graphical parameters for the vertical lines limiting each
#' episode. See \code{\link[graphics]{plot.default}}.
#' @examples
#' \dontrun{
#' # Read file "a03" from the physionet apnea-ecg database
#' library(RHRV)
#' HRVData <- CreateHRVData()
#' HRVData <- LoadBeatWFDB(HRVData,RecordName="test_files/WFDB/a03")
#' HRVData <- LoadApneaWFDB(HRVData,RecordName="test_files/WFDB/a03")
#' # Add other type of episode for a more complete example (this episode does
#' # not have any physiological meaning)
#' HRVData <- AddEpisodes(HRVData,InitTimes=c(4500),Durations=c(1000), 
#'                        Tags="Other", Values = 1)
#' HRVData <- BuildNIHR(HRVData)
#' HRVData <- FilterNIHR(HRVData)
#' HRVData <- InterpolateNIHR(HRVData)
#' 
#' 
#' PlotHR(HRVData)
#' OverplotEpisodes(HRVData,ymark=c(150,151),eplim=c(20,150))
#' 
#' # Change some default parameters
#' PlotHR(HRVData)
#' OverplotEpisodes(HRVData,ymark=c(150,151),eplim=c(20,150),
#'                  epLegendCoords=c(25000,150), lty=5, 
#'                  epColorPalette=c("blue","green"))
#'                  
#' # Use episodic information with the spectrogram... In order to obtain a proper
#' # representation of the episodes we need to avoid the use of the spectrogram
#' # legend
#' sp <- PlotSpectrogram(HRVData, size=600, shift=60, freqRange=c(0,0.05),
#'                       showLegend=F);
#' OverplotEpisodes(HRVData, markEpisodes=T, ymark=c(0.04,0.0401),
#'                  eplim=c(0,0.04), Tags="APNEA",
#'                  epColorPalette = c("white"), lwd=3)
#'}
OverplotEpisodes <- function(HRVData, Tags = NULL, Indexes=NULL, epColorPalette = NULL,
                             eplim, lty= 2, markEpisodes = T, ymark,
                             showEpLegend = T, epLegendCoords = NULL, Tag=NULL, ...){
  
  Tags = HandleDeprecatedTagArgument(Tag = Tag, Tags = Tags)
  
  if (is.null(Tags) && is.null(Indexes)) {
    stop("No episodes specified in OverplotEpisodes")
  }
  CheckEpisodes(HRVData)
    
  
  EpisodesToPlot <- selectEpisodes(HRVData$Episodes,Tags,Indexes)
  
  EpisodesToPlot <- EpisodesToPlot[EpisodesToPlot$selected,] 
  
  
  # Data for representing episodes
  
  episodesInitTime = EpisodesToPlot$InitTime
  episodesEndTime = EpisodesToPlot$InitTime + EpisodesToPlot$Duration
  episodesType = EpisodesToPlot$Type
  
  labels <- levels(factor(episodesType,levels=episodesType))
  
  VerboseMessage(HRVData$Verbose, paste("No of episodes:",length(episodesInitTime)))
  
  if (is.null(epColorPalette)){
    epColorPalette = rainbow(length(labels))
  }
  episodeColors = epColorPalette[match(episodesType, labels)]  
  
  
  if (markEpisodes){
    rect(episodesInitTime, ymark[1], episodesEndTime, ymark[2],
         border = episodeColors, col = episodeColors)
  }
  
  for (i in seq_along(episodesInitTime)) {
    lines(rep(episodesInitTime[i], times=2), c(eplim[1], eplim[2]), lty = lty, 
          col = episodeColors[i], ...)
    lines(rep(episodesEndTime[i], times=2), c(eplim[1], eplim[2]), lty = lty, 
          col = episodeColors[i], ...)
  }
  
  if (showEpLegend){
    if (is.null(epLegendCoords)){
      legend("top", 
             legend=labels, fill=episodeColors, cex=0.9, ncol = length(labels), 
             xjust=0.5, yjust=-0.2, bty="n")  
    }else{
      legend(x=epLegendCoords[1], epLegendCoords[2],
             legend=labels, fill=episodeColors, cex=0.9, ncol = length(labels), 
             xjust=0.5, yjust=-0.2, bty="n")  
    }
    
  }
  
  
  
}
