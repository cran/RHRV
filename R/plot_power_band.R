`plot_power_band` <-
function(power,lsecs,normalized=FALSE,hr=NA,ymax=160000,ymaxratio=10,ymaxnorm=1) {
# power: calculated by power_band()
# lsecs in seconds for x axis
# normalized: plots normalized powers if TRUE
# hr heart rate signal
# ymax: maximum value for y axis (unnormalized plots)
# ymaxratio: maximum value for y axis in LF/HF band (normalized and unnormalized plots)

   
# normalization: power/(sumpower-VLFpower)
   if (normalized == TRUE) {
#     sumpower=sum(power[,4])
      for (i in 1:dim(power)[1]) {
#         power[i,1]=power[i,1]/(sumpower-power[i,1])
#         power[i,2]=power[i,2]/(sumpower-power[i,1])
#         power[i,3]=power[i,3]/(sumpower-power[i,1])
#         power[i,4]=power[i,4]/(sumpower-power[i,1])
         power[i,1]=power[i,1]/power[i,4]
         power[i,2]=power[i,2]/power[i,4]
         power[i,3]=power[i,3]/power[i,4]
      }
   }
      
# For normalized plots, axis y is in the interval (0-ymax)
   if (normalized == FALSE) {
      ymaxv=c(0,ymax)
   }
   else {
      ymaxv=c(0,ymaxnorm)
   }
   
   ymaxratiov=c(0,ymaxratio)
   
   if (is.na(hr))
	   numfilas=4
   else
      numfilas=5
	   
   par(mfrow=c(numfilas,1),omi=c(0.1,0,0.1,0),mai=c(.5,0.5,0.3,0.1))
   
   mfg=c(1,2,numfilas,1)
   plot(seq(from=0,to=lsecs,length.out=dim(power)[1]),
        power[,2]/power[,3],type='l',xlab="",ylab="LF/HF",ylim=ymaxratiov)
   mfg=c(1,2,numfilas,1)
   plot(seq(from=0,to=lsecs,length.out=dim(power)[1]),
        power[,1],type='l',xlab="",ylab="VLF",ylim=ymaxv)
   mfg=c(1,3,numfilas,1)
   plot(seq(from=0,to=lsecs,length.out=dim(power)[1]),
        power[,2],type='l',xlab="",ylab="LF",ylim=ymaxv)
   mfg=c(1,4,numfilas,1)
   if (numfilas == 4) 
      texto4="No. of frames"
   else
      texto4=""
   plot(seq(from=0,to=lsecs,length.out=dim(power)[1]),
        power[,3],type='l',xlab=texto4,ylab="HF",ylim=ymaxv)
   
   if (numfilas==5) {
      mfg=c(1,5,numfilas,1)
      plot(seq(from=0,to=lsecs,length.out=dim(hr)[1]),
           hr[,2],type='l',xlab="Time (sec.)",ylab="HRV")
   }
   
   if (normalized == TRUE)
      title(main="Normalized power bands of HRV",outer=TRUE)
   else
      title(main="Power bands of HRV",outer=TRUE)
   
}

