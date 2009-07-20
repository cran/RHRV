`BuildNIHR` <-
function(HRVData,verbose=FALSE) {
#------------------------------------------------------ 
# Obtains instantaneous heart rate from beats positions
#------------------------------------------------------ 
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Calculating non-interpolated heart rate **\n")
	}

	if (is.null(HRVData$Beat$Time)) {
		cat("   --- ERROR: Beats positions not present... Impossible to calculate Heart Rate!! ---\n")
		return(HRVData)
	}
	
	NBeats=length(HRVData$Beat$Time)
	if (verbose) {
		cat("   Number of beats:",NBeats,"\n");
	}
	
	hr=c(0)
	hr[2:NBeats]=60.0/(HRVData$Beat$Time[2:NBeats]-HRVData$Beat$Time[1:NBeats-1])
	hr[1]=hr[2] # Not a real data
	HRVData$Beat$niHR = hr
	return(HRVData)
}

