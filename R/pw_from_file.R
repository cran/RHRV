`pw_from_file` <-
function(file,size,shift,fsamp=4) {
# Obtains power per band from a file
#    size, disp: size and displacement of window (sec.)
#    fsamp: sampling frequency
      
   message=c("  Reading beats positions from",file,".. ")
   cat(message)
   beat=load_beat_file(file)
   cat("Ok\n")
   
   cat("  Building the instantaneous heart rate signal from beats positions .. ")
   hr=build_hr(beat)
   cat("Ok\n")

   cat("  Filtering artifacts .. ")
   hr2=hr_filter(hr)
   cat("Ok\n")
   
   cat("  Interpolating for building the sample heart rate signal ..")
   hr3=hr_interpolator(hr2)
   cat("Ok\n")

   cat("  Calculating spectrogram ..")
   specgr=spectrogram(hr3[,2],size,shift)
   cat("Ok\n")
   
   cat("  Obtaining power per band ..")
   pw=power_band(specgr)
   cat("Ok\n")
   
   return(pw)

}

