`PlotSpectrogram` <-
function(HRVData,size,shift,sizesp=1024,scale="linear",verbose=FALSE) {
# -----------------
# Plots spectrogram
# -----------------
#    size, disp: size and displacement of window (sec.)
#    sizesp: seconds for calculating spectrogram (zero padding)
#	 scale: linear or logarithmic

	if (verbose) {
    	cat("** Plotting spectrogram **\n")
	}
	
	specgr=CalculateSpectrogram(HRVData,size,shift,sizesp,verbose)
  
	if(scale=="logaritmic")
	specgr=log(specgr)
	image(seq(from=0,,length.out=dim(specgr)[1]),
		seq(from=0,to=HRVData$Freq_HR/2,length.out=dim(specgr)[2]),
		specgr,
		xlab="No. of frames", ylab="Frequency (Hz.)",
		col=gray((256:0)/256)
	)
	if (verbose) {
		cat("   Spectrogram plotted\n")
	}
}

