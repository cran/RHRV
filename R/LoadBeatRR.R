LoadBeatRR <- function (HRVData, RecordName, RecordPath=".", scale = 1, datetime = "1/1/1900 0:0:0", verbose = NULL) {
#-------------------------------
# Loads beats from a RR ASCII file
#-------------------------------
#	RecordName -> file containing RR values
#	RecordPath -> path
#-------------------------------

  HRVData = HandleVerboseArgument(HRVData, verbose)
  
  VerboseMessage(HRVData$Verbose, 
                 paste("Loading beats positions for record:", RecordName))
  
  dir = getwd()
  on.exit(setwd(dir))
  
  VerboseMessage(HRVData$Verbose, paste("Path:", RecordPath))
  VerboseMessage(HRVData$Verbose, paste("Scale:", scale))
  
  setwd(RecordPath)
  
  x = read.table(RecordName)
  beats=cumsum(c(0,x$V1))
  
  datetimeaux = strptime(datetime, "%d/%m/%Y %H:%M:%S")
  if (is.na(datetimeaux)) {
    stop("Date/time format is dd/mm/yyyy HH:MM:SS")
  }
  VerboseMessage(HRVData$Verbose,
                 c(paste0("Date: ", sprintf("%02d", datetimeaux$mday), "/", 
                          sprintf("%02d", 1 + datetimeaux$mon), "/", 1900 + 
                            datetimeaux$year, "\n"),
                   paste0("Time: ", sprintf("%02d", datetimeaux$hour), ":", 
                          sprintf("%02d", datetimeaux$min), ":", 
                          sprintf("%02d", datetimeaux$sec))
                 ))
  HRVData$datetime = datetimeaux
  
  HRVData$Beat = data.frame(Time = beats * scale)
  VerboseMessage(HRVData$Verbose, 
                 paste("Number of beats:", length(HRVData$Beat$Time)))
  
  return(HRVData)
}
