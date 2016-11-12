FilterNIHR <-
function(HRVData, long=50, last=13, minbpm=25, maxbpm=200, mini=NULL, maxi=NULL, fixed=NULL, verbose=NULL) {
# ----------------------------------------------------------------------------------------
# Filters non-interpolated heart rate
# Filtering is based on comparisons with previous and last values and with an updated mean
# ----------------------------------------------------------------------------------------
	
  CheckDeprecatedArg(mini)                  
  CheckDeprecatedArg(maxi)                  
  CheckDeprecatedArg(fixed)                  
  
  HRVData = HandleVerboseArgument(HRVData, verbose)
  
  CheckBeats(HRVData)
  CheckNIHR(HRVData)
  
  VerboseMessage(HRVData$Verbose, "Filtering non-interpolated Heart Rate")
  VerboseMessage(HRVData$Verbose,
                 paste("Number of original beats:",length(HRVData$Beat$niHR)))
	
		
	n=length(HRVData$Beat$niHR)
	lon=50
	last=13
	ind=seq(from=0,to=0,length.out=n)
	minbpm=25
	maxbpm=200
	# call C function to load the results in out
	out <-.C("filterhr",hr=as.double(HRVData$Beat$niHR),as.integer(n),as.integer(lon),as.integer(last),as.integer(minbpm),as.integer(maxbpm),ind=as.integer(ind))
	#copiei a maneira que implmentou Leandro para actualizar a estructura HRVData$Beat
	hr=HRVData$Beat$niHR[out$ind==1]
	beat=HRVData$Beat$Time[out$ind==1]
	rr=HRVData$Beat$RR[out$ind==1]
	
	VerboseMessage(HRVData$Verbose, 
	               paste("Number of accepted beats:", length(hr)))
	
	
	HRVData$Beat = data.frame (Time=beat, niHR=hr, RR=rr)
	return(HRVData)
}

