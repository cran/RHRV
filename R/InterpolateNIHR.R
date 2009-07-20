`InterpolateNIHR` <-
function(HRVData,freqhr=4,verbose=FALSE) {
# -------------------------------------
# Interpolates instantaneous heart rate
# -------------------------------------
#	Verbose -> TRUE for verbose mode
#	freqhr -> frequency for interpolating heart rate

	if (verbose) {
		cat("** Interpolating instantaneous heart rate **\n");
		cat("   Frequency: ",freqhr,"Hz\n",sep="")
	}
	
	HRVData$Freq_HR=freqhr

	first = head(HRVData$Beat$Time,1)
	last = tail(HRVData$Beat$Time,1)
 	npoints=as.integer((last-first)*HRVData$Freq_HR)
	vectorx = array(1:npoints)
	vectory = array(1:npoints)

	if (verbose) {
		cat("   Number of beats:",length(HRVData$Beat$niHR),"\n")
		cat("   Number of points:",npoints,"\n");
	}

  	# limit indicates the maximum width in seconds of an interval without beats,
	# if the interval between two consecutive beats is greater than limit then
	# some points are created to avoid the appearance of incoherent splines 
  	limit=7
  	vectorx[1]=HRVData$Beat$Time[1]
  	vectory[1]=HRVData$Beat$niHR[1]
  	j=2
  	for (i in 2:length(HRVData$Beat$Time)) {
   		if ((HRVData$Beat$Time[i] - HRVData$Beat$Time[i-1]) < limit) {
     		vectorx[j]=HRVData$Beat$Time[i]
      		vectory[j]=HRVData$Beat$niHR[i]
      		j=j+1
    	} else {
      		npoints2=as.integer((HRVData$Beat$Time[i] - HRVData$Beat$Time[i-1])/limit)
      		vectorx[j+npoints2+1]=HRVData$Beat$Time[i]
      		vectory[j+npoints2+1]=HRVData$Beat$niHR[i]
      		for (k in j:(j+npoints2)) {
        		vectorx[k]=vectorx[k-1]+limit
        		vectory[k]=vectory[k-1]+(HRVData$Beat$niHR[i] - HRVData$Beat$niHR[i-1])/(npoints2+1)
      		}
      	j=j+npoints2+2;
    	}      
  	}
  	vectorx=vectorx[1:j-1]
  	vectory=vectory[1:j-1]
  
  	hr=spline(vectorx,vectory,n=npoints,method="natural",xmin=first,xmax=last)

  	iHR=as.numeric(hr$y)
    
	HRVData$HR=(iHR)
	
	return(HRVData)
}

