BuildNIHR <-
function(HRVData, verbose=NULL) {
#------------------------------------------------------ 
# Obtains instantaneous heart rate from beats positions
#------------------------------------------------------ 

  HRVData = HandleVerboseArgument(HRVData, verbose)
  
  VerboseMessage(HRVData$Verbose, "Calculating non-interpolated heart rate")
  
  # Check if some beats have been loaded
  CheckBeats(HRVData)
  
  NBeats = length(HRVData$Beat$Time)
	VerboseMessage(HRVData$Verbose, paste("Number of beats:",NBeats))
	
	hr = c(0)
	hr[2:NBeats] = 60.0 / diff(HRVData$Beat$Time)
	hr[1] = hr[2] # Not a real data
	HRVData$Beat$niHR = hr
	
	rr = c(0)
	rr[2:NBeats] = 1000.0 * diff(HRVData$Beat$Time)
	rr[1] = rr[2] # Not a real data
	HRVData$Beat$RR = rr
	
	return(HRVData)
}

