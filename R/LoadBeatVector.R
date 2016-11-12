#' Loads beats positions from an R vector
#' @description Stores the beat positions from an R vector under the
#' \emph{HRVData} data structure.
#' @param HRVData Data structure that stores the beats recording and information 
#' related to it
#' @param beatPositions Numeric vector with the heartbeats occurrence's times since
#' the beginning of the recording. See \emph{scale} parameter to specify the 
#' units
#' @param scale Numeric value identifying the temporal units in which
#' the beat positions are specified: 1 if beat positions is specified in seconds, 
#'  0.001 if beat positions in milliseconds, etc.
#' @param datetime Date and time (DD/MM/YYYY HH:MM:SS) of the beginning of the
#' recording 
#' @examples
#' \dontrun{
#' hd = CreateHRVData()
#' hd = LoadBeatVector(hd, 
#'      c(0.000, 0.328, 0.715, 0.124, 1.50,1.880, 2.268, 2.656))
#' hd = BuildNIHR(hd)
#' # ... continue analyzing the recording
#' }
#' @return  A \emph{HRVData} structure containing the heartbeat positions
#' from the \emph{beatPositions} vector.
LoadBeatVector <- function(HRVData, beatPositions, scale = 1,
                           datetime = "1/1/1900 0:0:0"){
  VerboseMessage(HRVData$Verbose, "Loading beats positions")
  dir = getwd()
  on.exit(setwd(dir))
  VerboseMessage(HRVData$Verbose, paste("Scale:", scale))
  
  beatsaux = beatPositions
  beatPositions = beatsaux[!duplicated(beatsaux)]
  if (length(beatsaux) != length(beatPositions)) {
    warning(paste("Removed", length(beatsaux) - length(beatPositions), 
                  "duplicated beat positions"))
  }
  datetimeaux = strptime(datetime, "%d/%m/%Y %H:%M:%S")
  if (is.na(datetimeaux)) {
    stop("Date/time format is dd/mm/yyyy HH:MM:SS")
  }
  VerboseMessage(HRVData$Verbose,
                 paste("Date: ", sprintf("%02d", datetimeaux$mday), "/", 
                       sprintf("%02d", 1 + datetimeaux$mon), "/", 1900 + 
                         datetimeaux$year, sep = ""))
  VerboseMessage(HRVData$Verbose, 
                 paste("Time: ", sprintf("%02d", datetimeaux$hour), ":", 
                       sprintf("%02d", datetimeaux$min), ":", 
                       sprintf("%02d",  datetimeaux$sec), sep = ""))
  
  HRVData$datetime = datetimeaux
  HRVData$Beat = data.frame(Time = beatPositions * scale)
  VerboseMessage(HRVData$Verbose, 
                 paste("Number of beats:", length(HRVData$Beat$Time)))
  
  return(HRVData)
}