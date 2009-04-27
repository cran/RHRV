`InterpolateNIHR` <-
function(Data,freqhr=4,verbose=FALSE) {
	#	Verbose -> TRUE for verbose mode
	#	freqhr -> frequency for interpolating heart rate

	if (verbose) {
		cat("** Interpolating instantaneous heart rate **\n");
		cat("   Frequency: ",freqhr,"Hz\n",sep="")
	}
	
	Data$Freq_HR=freqhr
	
	vectorx = Data$Beat$Time
	vectory = Data$Beat$niHR
	first = head(Data$Beat$Time,1)
	last = tail(Data$Beat$Time,1)
  	npoints=as.integer((last-first)*Data$Freq_HR)

	if (verbose) {
		cat("   Number of beats:",length(Data$Beat$niHR),"\n")
		cat("   Number of points:",npoints,"\n");
	}

	hr=spline(vectorx,vectory,n=npoints,method="natural",xmin=first,xmax=last)

    iHR=as.numeric(hr$y)
	Data$HR=(iHR)
	
	return(Data)
}

