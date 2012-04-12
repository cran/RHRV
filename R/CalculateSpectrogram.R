CalculateSpectrogram <-
function(HRVData, size, shift, sizesp=1024, verbose=NULL) {
# ---------------------------------------------------------------
# Calculates the spectrogram of an interpolated heart rate signal
# ---------------------------------------------------------------
#    size, disp: size and displacement of window (sec.)
#    sizesp: points for calculating spectrogram (zero padding)
# Used by PlotSpectrogram and CalculatePowerPerBand

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("   Calculating spectrogram\n")
	}
	
	shift=shift*HRVData$Freq_HR
	size=floor(size*HRVData$Freq_HR)
	if (HRVData$Verbose) {
		cat("      Window: ",size," samples (shift ",shift," samples)\n",sep="")
	}
	
	
	if (sizesp < size)
	{
 		stop("  --- Window bigger than points considered for spectrogram !! ---\n    --- Quitting now!! ---\n")
	}
	
	sizezp=sizesp-size
	if (HRVData$Verbose) {
		cat("      Window size for calculation: ",sizesp," samples (zero padding: ",sizezp," samples)\n",sep="")
	}
	
	signal=HRVData$HR/60
	if (HRVData$Verbose) {
	
		cat("      Signal size: ",length(signal)," samples\n",sep="")
	}
	hamming=0.54-0.46*cos(2*pi*(0:(size-1))/(size-1))
	
	
	# Calculates the number of windows
	nw=1
	begnw=1
	repeat {
    	begnw=begnw+shift
    	if ((begnw+size-1)>=length(signal)) {
      		break
    	}
		nw=nw+1
	}
	
	if (HRVData$Verbose) {
		cat("      Windowing signal... ",nw," windows \n",sep="")
	}
	
	zp=matrix(nrow=nw,ncol=sizesp)
	for (i in 1:nw) {
		beg=1+(shift*(i-1))
		kk = signal[beg:(beg + size - 1)]
        kk = kk - mean(kk)
        zp[i, ] = c(kk * hamming, seq(from = 0, to = 0, length = sizezp))
	}
	
	z=matrix(nrow=nw,ncol=floor(sizesp/2))
	for (i in 1:nw) {
    	f=abs(fft(zp[i,]))^2
    	f=f[1:(length(f)/2)]
    	z[i,]=f
	}
	
	if (HRVData$Verbose) {
		cat("      Spectrogram calculated\n")
	}

	return(z)
}

