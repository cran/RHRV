`CalculatePowerBand` <-
function(Data,size,shift,sizesp=1024,scale="linear", ULFmin=0, ULFmax=0.03, VLFmin=0.03, VLFmax=0.05, LFmin=0.05, LFmax=0.15, HFmin=0.15, HFmax=0.4,verbose=FALSE) {
	# Calculates power per band
#   size, disp: size and displacement of window (sec.)
#   sizesp: seconds for calculating spectrogram (zero padding)
#	ULF band: from 0 to 0.03Hz
# 	VLF band: from 0.03 to 0.05Hz
# 	LF band: from 0.05 to 0.15Hz
# 	HF band: from 0.15 to 0.4Hz

	if (verbose) {
		cat("** Calculating power per band **\n")
	}
  
	specgr=CalculateSpectrogram(Data,size,shift,sizesp,verbose)

	num_frames=dim(specgr)[1]
	num_points=dim(specgr)[2]
	fsamp=Data$Freq_HR

	if (verbose) {
		cat("   Number of frames: ",num_frames, "\n")
		cat("   Number of points: ",num_points, "\n")
	}
  
  	# absolute power per band
	Data$Param=data.frame(ULF=array(dim=num_frames),VLF=array(dim=num_frames),LF=array(dim=num_frames),HF=array(dim=num_frames),LFHF=array(dim=num_frames),HRV=array(dim=num_frames))

	for (i in 1:num_frames) {
		if ((ULFmin*num_points/(fsamp/2)) <= 2) {
			Data$Param$ULF[i]=sum(specgr[i,2:(ULFmax*num_points/(fsamp/2))]) #ULF
		} else {
			Data$Param$ULF[i]=sum(specgr[i,(ULFmin*num_points/(fsamp/2)):(ULFmax*num_points/(fsamp/2))]) #ULF
		}
		
		if ((VLFmin*num_points/(fsamp/2)) <= 2) {
			Data$Param$VLF[i]=sum(specgr[i,2:(VLFmax*num_points/(fsamp/2))]) #VLF
		} else {
			Data$Param$VLF[i]=sum(specgr[i,(VLFmin*num_points/(fsamp/2)):(VLFmax*num_points/(fsamp/2))]) #VLF
		}
		Data$Param$LF[i]=sum(specgr[i,(LFmin*num_points/(fsamp/2)):(LFmax*num_points/(fsamp/2))]) #LF
		Data$Param$HF[i]=sum(specgr[i,(HFmin*num_points/(fsamp/2)):(HFmax*num_points/(fsamp/2))]) #HF
    	Data$Param$LFHF[i]=Data$Param$LF[i]/Data$Param$HF[i] #LF/HF
		Data$Param$HRV[i]=sum(specgr[i,2:dim(specgr)[2]]) #Total
	}
  
	if (verbose) {
		cat("   Power per band calculated\n")
	}
	return(Data)
}

