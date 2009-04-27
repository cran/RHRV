`CalculateSpectrogram` <-
function(Data,size,shift,sizesp=1024,verbose=FALSE) {
# Calculates the spectrogram of a signal
#    size, disp: size and displacement of window (sec.)
#    sizesp: seconds for calculating spectrogram (zero padding)
# Used by PlotSpectrogram and CalculatePowerPerBand

	if (verbose) {
		cat("   Calculating spectrogram\n")
	}
	
	shift=shift*Data$Freq_HR
	size=size*Data$Freq_HR
	if (verbose) {
		cat("      Window: ",size," samples (shift ",shift," samples)\n",sep="")
	}
	
	sizesp=sizesp*Data$Freq_HR
	
	if (sizesp < size)
	{
 		cat("      --- ERROR: window bigger than points considered for spectrogram !! ---\n")
		return(invisible())
	}
	
	sizezp=sizesp-size
	if (verbose) {
		cat("      Window size for calculation: ",sizesp," samples (zero padding: ",sizezp," samples)\n",sep="")
	}
	
	signal=Data$HR*1000/60
	if (verbose) {
		cat("      Signal size: ",length(signal)," samples\n",sep="")
	}
	
	signal=signal-mean(signal)	
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
	
	if (verbose) {
		cat("      Windowing signal... ",nw," windows \n",sep="")
	}
	
	zp=matrix(nrow=nw,ncol=sizesp)
	for (i in 1:nw) {
		beg=1+(shift*(i-1))
		zp[i,] = c(signal[beg:(beg+size-1)]*hamming,seq(from=0,to=0,length=sizezp))
	}
	
	z=matrix(nrow=nw,ncol=floor(sizesp/2))
	for (i in 1:nw) {
    	f=Mod(fft(zp[i,]))
    	f=f[1:(length(f)/2)]
    	z[i,]=f
	}
	
	if (verbose) {
		cat("      Spectrogram calculated\n")
	}
	return(z)
}

