InterpolateNIHR <-
function(HRVData, freqhr=4, verbose=FALSE) {
# -------------------------------------
# Interpolates instantaneous heart rate
# -------------------------------------
#	freqhr -> frequency for interpolating heart rate

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Interpolating instantaneous heart rate **\n");
		cat("   Frequency: ",freqhr,"Hz\n",sep="")
	}
	
	HRVData$Freq_HR=freqhr

	first = head(HRVData$Beat$Time,1)
	last = tail(HRVData$Beat$Time,1)
 	npoints=as.integer((last-first)*HRVData$Freq_HR+1)

	if (HRVData$Verbose) {
		cat("   Number of beats:",length(HRVData$Beat$niHR),"\n")
		cat("   Number of points:",npoints,"\n");
	}

  	spf=splinefun(HRVData$Beat$Time,HRVData$Beat$niHR,method="monoH.FC",ties="ordered")

	vectorxint=seq(first,last,1/HRVData$Freq_HR)
    
	HRVData$HR=spf(vectorxint)
	
	return(HRVData)
}

