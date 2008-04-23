`plot_spectrogram` <-
function(specgr,fsamp=4,scale="linear") {
# Plots spectrogram
#    specgr: spectrogram calculated with spectrogram()
#    fsamp: sampling frequency
   if(scale=="logaritmic")
	specgr=log(specgr)
   image(seq(from=0,,length.out=dim(specgr)[1]),
         seq(from=0,to=fsamp/2,length.out=dim(specgr)[2]),
         specgr,
         xlab="No. of frames", ylab="Frequency (Hz.)"
         ,col=gray((256:0)/256)
         )
}

