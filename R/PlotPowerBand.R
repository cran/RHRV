`PlotPowerBand` <-
function(Data,normalized=FALSE,hr=FALSE,ymax=160000,ymaxratio=10,ymaxnorm=1,Episodes=FALSE,verbose=FALSE) {
# 	normalized: plots normalized powers if TRUE
# 	hr: plots heart rate signal if TRUE
# 	ymax: maximum value for y axis (unnormalized plots)
# 	ymaxratio: maximum value for y axis in LF/HF band (normalized and unnormalized plots)
#	Episodes -> TRUE for representing episodes
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Plotting power per band **\n")
	}

   if (Episodes & is.null(Data$Episodes)) {
		cat("   --- ERROR: Episodes not present!! ---\n")
		return(invisible())
   }
	
	if (is.null(Data$Param$ULF)) {
		cat("   --- ERROR: Power per band not present!! ---\n")
		return(Data)
	}
	 
	# normalization
	 if (normalized == TRUE) {
		Data$Param$ULF=(Data$Param$ULF-min(Data$Param$ULF))/(max(Data$Param$ULF)-min(Data$Param$ULF))
		Data$Param$VLF=(Data$Param$VLF-min(Data$Param$VLF))/(max(Data$Param$VLF)-min(Data$Param$VLF))
		Data$Param$LF=(Data$Param$LF-min(Data$Param$LF))/(max(Data$Param$LF)-min(Data$Param$LF))
		Data$Param$HF=(Data$Param$HF-min(Data$Param$HF))/(max(Data$Param$HF)-min(Data$Param$HF))
		# Data$Param$LFHF=(Data$Param$LFHF-min(Data$Param$LFHF))/(max(Data$Param$LFHF)-min(Data$Param$LFHF))
		if (verbose) {
			cat("   Power per band normalized\n")
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
	
	if (hr)
		numfilas=6
	else
		numfilas=5

	# lframes is the number of frames for plotting power per band
	lframes=length(Data$Param$HRV)

   # Episodes
   if (Episodes) {
      episodesLeft=Data$Episodes$InitTime # Beg of episodes (seconds)
      episodesLeftFrame=episodesLeft*lframes/(tail(Data$Beat$Time,1)-head(Data$Beat$Time,1)) # Beg of episodes (frames)
      episodesRight=Data$Episodes$InitTime+Data$Episodes$Duration # Beg of episodes (seconds)
      episodesRightFrame=episodesRight*lframes/(tail(Data$Beat$Time,1)-head(Data$Beat$Time,1)) # Beg of episodes (frames)
      cat("   No of episodes:",length(episodesLeft),"\n")
   }

	par(mfrow=c(numfilas,1),omi=c(0.1,0,0.1,0),mai=c(0.5,0.5,0.9,0.1),mar=c(3,5,1,2),oma=c(1,1,2,1))
	
	mfg=c(1,1,numfilas,1)
	plot(seq(from=0,to=lframes,length.out=length(Data$Param$HRV)),
			Data$Param$LFHF,type='l',xlab="",ylab="LF/HF",ylim=ymaxratiov)
   if (Episodes) {
      rect(episodesLeftFrame,rep(ymaxratiov[1],times=length(episodesLeft)),
         episodesRightFrame,rep(ymaxratiov[2],times=length(episodesLeft)),
         border="red")
   }
	if (verbose) {
		cat("   Plotted LF/HF\n")
	}
	mfg=c(1,2,numfilas,1)
	plot(seq(from=0,to=lframes,length.out=length(Data$Param$HRV)),
			Data$Param$ULF,type='l',xlab="",ylab="ULF",ylim=ymaxv)
   if (Episodes) {
      rect(episodesLeftFrame,rep(ymaxv[1],times=length(episodesLeft)),
         episodesRightFrame,rep(ymaxv[2],times=length(episodesLeft)),
         border="red")
   }
	if (verbose) {
		cat("   Plotted ULF\n")
	}
	mfg=c(1,3,numfilas,1)
	plot(seq(from=0,to=lframes,length.out=length(Data$Param$HRV)),
			Data$Param$VLF,type='l',xlab="",ylab="VLF",ylim=ymaxv)
   if (Episodes) {
      rect(episodesLeftFrame,rep(ymaxv[1],times=length(episodesLeft)),
         episodesRightFrame,rep(ymaxv[2],times=length(episodesLeft)),
         border="red")
   }
	if (verbose) {
		cat("   Plotted VLF\n")
	}
	mfg=c(1,4,numfilas,1)
	plot(seq(from=0,to=lframes,length.out=length(Data$Param$HRV)),
			Data$Param$LF,type='l',xlab="",ylab="LF",ylim=ymaxv)
   if (Episodes) {
      rect(episodesLeftFrame,rep(ymaxv[1],times=length(episodesLeft)),
         episodesRightFrame,rep(ymaxv[2],times=length(episodesLeft)),
         border="red")
   }
	if (verbose) {
		cat("   Plotted LF\n")
	}
	mfg=c(1,5,numfilas,1)
	texto4="No. of frames"
	plot(seq(from=0,to=lframes,length.out=length(Data$Param$HRV)),
			Data$Param$HF,type='l',xlab=texto4,ylab="HF",ylim=ymaxv)
   if (Episodes) {
      rect(episodesLeftFrame,rep(ymaxv[1],times=length(episodesLeft)),
         episodesRightFrame,rep(ymaxv[2],times=length(episodesLeft)),
         border="red")
   }
	if (verbose) {
		cat("   Plotted HF\n")
	} 

	if (numfilas==6) {
		mfg=c(1,6,numfilas,1)
		# lsecs is the duration of the record in seconds for plotting heart rate signal
		lsecs=tail(Data$Beat$Time,1)-head(Data$Beat$Time,1)
		plot(seq(from=0,to=lsecs,length.out=length(Data$HR)),
				 Data$HR,type='l',xlab="Time (sec.)",ylab="HRV")
		if (verbose) {
			cat("   Plotted HRV\n")
		}

      if (Episodes) {
         rect(episodesLeft,rep(min(Data$HR),times=length(episodesLeft)),
            episodesRight,rep(max(Data$HR),times=length(episodesLeft)),
               border="red")
      }
	}

   if (verbose & Episodes) {
      cat("   Episodes plotted\n")
   }
	
	if ((normalized == TRUE) && (numfilas == 6)) 
		title(main="Normalized power bands of HRV and Heart Rate Signal",outer=TRUE)
	else if ((normalized == FALSE) && (numfilas == 6))
		title(main="Power bands of HRV and Heart Rate Signal",outer=TRUE)
	else if ((normalized == TRUE) && (numfilas != 6))
		title(main="Normalized power bands of HRV",outer=TRUE)
	else
		title(main="Power bands of HRV",outer=TRUE)
	
	if (verbose) {
		cat("   Power per band plotted\n")
	}	 
}

