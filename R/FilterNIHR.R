`FilterNIHR` <-
function(Data,long=50,mini=12,maxi=20,fixed=10,last=13,verbose=FALSE) {
	#	Verbose -> TRUE for verbose mode
	
	if (verbose) {
		cat("** Filtering non-interpolated Heart Rate **\n")
		cat("   Number of original beats:",length(Data$Beat$niHR),"\n")
	}
	
	
	# threshold initialization
	ulast=last
	umean=1.5*ulast

	L=1 # index of last accepted beat

	count=0 # index for acepted beats
	
	N=length(Data$Beat$Time)
	beat=Data$Beat$Time
	beat2=beat
	hr=Data$Beat$niHR
	hr2=hr # array of accepted beats
	
	# main loop
	for (i in 2:N-1) {
		
		# mean is calculated for the last "long" beats
		if(i<=long)
			M=mean(hr[1:i])
		else
			M=mean(hr[i-long:i])
			
		# Rule for beat acceptation or rejection. Each value is compared with previous, following
		# and with the updated mean. We apply also a comparison with accepable physiological
		# values (25 and 200 bpm) 
		
		if((100*abs((hr[i]-hr[L])/hr[L]) < ulast |
	 	   100*abs((hr[i]-hr[i+1])/hr[i+1]) < ulast |
		    100*abs((hr[i]-M)/M) < umean) & hr[i]>24 & hr[i]<198) {
			
			# if beat is accepted index L and count are updated and the values are copied into hr2 
			L=i
			count=count+1
			beat2[count]=beat[i]
			hr2[count]=hr[i]
			
			# every "long" beats threshold values are updated	
			if(i%%long == 0){
				# threshold has a fixed component and other variable with standard deviation of last "long" beats 
				tmp=fixed+sd(hr[(i-long):i])
				# but never goes out of (mini,maxi) interval
				if(tmp < mini)
					tmp=mini
				if(tmp > maxi)
					tmp=maxi
					# ulast is the theshold for comparison with previous and following beats
					# umean for comparison with updated mean
				uutl=tmp
				umed=1.5*tmp
			}
		}
	} # main loop
	
	if (verbose) {
		cat("   Number of accepted beats:",count,"\n")
	}
	Data$Beat = data.frame (Time=beat2[1:count], niHR= hr2[1:count])
	return(Data)
}

