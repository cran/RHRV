`spectrogram` <-
function(signal,size,shift,fsamp=4) {
# Calculates the spectrogram of a signal
#    size, disp: size and displacement of window (sec.)
#    fsamp: sampling frequency
   beg=1
   shift=shift*fsamp
   size=size*fsamp
   signal=signal[,2]
   signal=signal-mean(signal)
   w=signal[beg:beg+size]
   repeat {
      beg=beg+shift
      if ((beg+size)>=length(signal)) {
         break
      }
      w=rbind(w,signal[beg:(beg+size)])
   }
   f=Mod(fft(w[1,]))
   z=f[1:(length(f)/2)]
   for (i in 2:dim(w)[1]) {
      f=Mod(fft(w[i,]))
   	f=f[1:(length(f)/2)]
      z=rbind(z,f)
   }
   return(z)
}

