`hr_filter` <-
function(hr,long=50,mini=12,maxi=20,fixed=10,last=13){
# artifact filter.based in comparisons with previous and last values and with an updated mean
#
# threshold initialization
	ulast=last
	umean=1.5*ulast
# L is the index of last accepted beat
	L=1
# count is an index for aceepted beats array
	count=0
# hr2 is the accepted beats array
	N=nrow(hr)
	hr2=hr
# main loop
	for (i in 2:N-1) {
# mean os last "long" beats
		if(i<long)
			M=mean(hr[1:i,2])
		else
			M=mean(hr[i-long:i,2])
# Rule for beat acceptation or rejection. Each values is compared with previous, following
# and with the updated mean. We apply also a comparison with accepable phisiliogical
# values (25 and 200 bpm) 
		if((100*abs((hr[i,2]-hr[L,2])/hr[L,2]) < ulast |
	 	   100*abs((hr[i,2]-hr[i+1,2])/hr[i+1,2]) < ulast |
		    100*abs((hr[i,2]-M)/M) < umean) & hr[i,2]>400 & hr[i,2]<3300) {
# if beat is accepted index L and count are updates and the values is copied into hr2 
			L=i
			count=count+1
			hr2[count,1]=hr[i,1]	
			hr2[count,2]=hr[i,2]
# every "lon" beats threshold values are updated	
			if(i%%long == 0){
# threshold has a fixed component and other variable with standard deviation of last "long" beats 
				tmp=fixed+sd(hr[(i-long):i,2])
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
	}
	return(hr2[1:count,1:2])
}

