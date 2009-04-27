`BuildNIHR` <-
function(Data,verbose=FALSE) {
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Calculating non-interpolated heart rate **\n")
	}

	if (is.null(Data$Beat$Time)) {
		cat("   --- ERROR: Beats positions not present... impossible to calculate Heart Rate!! ---\n")
		return(Data)
	}
	
	NBeats=length(Data$Beat$Time)
	if (verbose) {
		cat("   Number of beats:",NBeats,"\n");
	}
	
	hr=c(0)
	hr[2:NBeats]=60.0/(Data$Beat$Time[2:NBeats]-Data$Beat$Time[1:NBeats-1])
	hr[1]=hr[2] # Not a real data
	Data$Beat$niHR = hr
	return(Data)
}

