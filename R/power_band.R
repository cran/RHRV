`power_band` <-
function(specgr, fsamp=4) {
# Calculates power per band
# VLF band: from 0 to 0.05Hz
# LF band: from 0.05 to 0.15Hz
# HF band: from 0.15 to 0.4Hz
   num_frames=dim(specgr)[1]
   num_points=dim(specgr)[2]
   
# absolute power per band
   power=array(dim=c(num_frames,4))
   for (i in 1:num_frames) {
      power[i,1]=sum(specgr[i,2:(0.05*num_points/(fsamp/2))]) #VLF
      power[i,2]=sum(specgr[i,(0.05*num_points/(fsamp/2)):(0.15*num_points/(fsamp/2))]) #LF
      power[i,3]=sum(specgr[i,(0.15*num_points/(fsamp/2)):(0.4*num_points/(fsamp/2))]) #HF
      power[i,4]=sum(specgr[i,2:dim(specgr)[2]]) #Total
   }
   
   
   
   return(power)
}

