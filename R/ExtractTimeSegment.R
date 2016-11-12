############################## ExtractTimeSegment #########################################
#' Time windows of HR record
#' @description
#' Extracts a temporal subset between the times starttime and endtime. 
#' @details 
#' If the \emph{HRVData} contains episodes, beats or RR time series,
#' these will be also extracted into the new HRV structure. On the other hand,
#' all the analysis stored in the original structure will be lost.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it. This function calls \emph{Window} to perform the extraction.
#' @param starttime The start time of the period of interest.
#' @param endtime The end time of the period of interest.
#' @return A new \emph{HRVData} structure containing the temporal data  
#' within the specified range.  
#' @author Leandro Rodriguez-Linares
#' @examples
#' \dontrun{
#' data(HRVProcessedData)
#' # Rename for convenience
#' HRVData <- HRVProcessedData
#' PlotNIHR(HRVData)
#' newHRVData <- ExtractTimeSegment(HRVData,2000,4000)
#' PlotNIHR(newHRVData)
#' }
ExtractTimeSegment <- function(HRVData, starttime, endtime) {
  Segment <- Window(HRVData, starttime, endtime)
  Segment
}


############################## Window #########################################
#' Time windows of RR intervals
#' @description
#' Extracts a temporal subset between the times start and end. 
#' @details 
#' If the \emph{HRVData} episodes, beats or RR time series,
#' these will be also extracted into the new HRV structure. On the other hand,
#' all the analysis stored in the original structure will be lost.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param start The start time of the period of interest.
#' @param end The end time of the period of interest.
#' @return A new \emph{HRVData} structure containing the subset of RR intervals
#' within the specified range.  
#' @examples
#' \dontrun{
#' data(HRVProcessedData)
#' # Rename for convenience
#' HRVData <- HRVProcessedData
#' PlotNIHR(HRVData)
#' newHRVData <- Window(HRVData,2000,4000)
#' PlotNIHR(newHRVData)
#' }
Window = function(HRVData, start, end){
  # Check if some beats have been loaded  
  CheckBeats(HRVData)
  # check proper window definition
  if (start > end){
    stop("'start' cannot be after 'end'")
  } 
  
  # Create empty new HRVData
  newHRVData = CreateHRVData()
  newHRVData$datetime = HRVData$datetime
  newHRVData$Verbose = HRVData$Verbose
  # indx inside the window
  indx = which( (HRVData$Beat$Time >= start) & (HRVData$Beat$Time <= end) )
  if ( length(indx) == 0){
    stop("Window out of range: No values were selected")
  }else{
    newHRVData$Beat = HRVData$Beat[indx,,drop=FALSE]
  }
  # If HRVData has interpolated data, we also interpolate in newHRVData
  if( !is.null(HRVData$HR) ){
    interpTime = seq(HRVData$Beat$Time[1], tail(HRVData$Beat$Time, 1), 
                     len=length(HRVData$HR))
    indx = which( (interpTime >= start) & (interpTime <= end) )
    newHRVData$Freq_HR = HRVData$Freq_HR
    newHRVData$HR = HRVData$HR[indx]
  }
  # Finally include episodes
  WindowEpisodes(newHRVData,HRVData,start,end)

}

# Private function: copies proper episodes from HRVData to newHRVData
WindowEpisodes = function(newHRVData,HRVData,start,end){
  if (!is.null(HRVData$Episodes)){
    episodes = SelectWindowEpisodes(HRVData,start,end)
    if (nrow(episodes) > 0){
      newHRVData$Episodes = episodes
    }
  }
  newHRVData
}

# Private function: selects proper episodes from HRVData on the basis of start
# and end 
SelectWindowEpisodes = function(HRVData,start,end){
  # This should not be necessary
  if (is.null(HRVData$Episodes)){ return(NULL) }
  # The episodes to drop are those that begin after the time 'end'
  # and those that end before time 'start'.
  endTimes = HRVData$Episodes$InitTime + HRVData$Episodes$Duration
  indx = which( (HRVData$Episodes$InitTime) > end | (endTimes < start))
  if (length(indx) > 0){
    # eliminate them
    HRVData$Episodes[-indx,]
  }else{
    HRVData$Episodes
  }
}
