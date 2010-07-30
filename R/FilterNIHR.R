FilterNIHR <-
function(HRVData, long=50, last=13, mini=NULL, maxi=NULL, fixed=NULL, verbose=NULL) {
# ----------------------------------------------------------------------------------------
# Filters non-interpolated heart rate
# Filtering is based on comparisons with previous and last values and with an updated mean
# ----------------------------------------------------------------------------------------
	
	if (!is.null(mini)) {
		cat("  --- Warning: deprecated argument ignored ---\n    --- See help for more information!! ---\n")
	}
	
	if (!is.null(maxi)) {
		cat("  --- Warning: deprecated argument ignored ---\n    --- See help for more information!! ---\n")
	}
	
	if (!is.null(fixed)) {
		cat("  --- Warning: deprecated argument ignored ---\n    --- See help for more information!! ---\n")
	}
	
	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Filtering non-interpolated Heart Rate **\n")
		cat("   Number of original beats:",length(HRVData$Beat$niHR),"\n")
	}
		
	# threshold initialization
	ulast=last
	umean=1.5*ulast
   	hr=HRVData$Beat$niHR
	beat=HRVData$Beat$Time
	rr=HRVData$Beat$RR

   	index=2

   	while (index<length(hr)) {
      	v = hr[max(index-long,1):index-1]
      	M = sum(v)/length(v)

      	if((100*abs((hr[index]-hr[index-1])/hr[index-1]) < ulast | 100*abs((hr[index]-hr[index+1])/hr[index+1]) < ulast | 100*abs((hr[index]-M)/M) < umean) & hr[index]>24 & hr[index]<198) {
        	index=index+1
         } 

      	else {
         	hr=hr[-index]
         	beat=beat[-index]
         	rr=rr[-index]
      	}
   	}

	if (HRVData$Verbose) {
		cat("   Number of accepted beats:",length(hr),"\n")
	}
	
	HRVData$Beat = data.frame (Time=beat, niHR=hr, RR=rr)
	return(HRVData)
}

