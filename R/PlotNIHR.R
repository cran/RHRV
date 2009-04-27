`PlotNIHR` <-
function(Data,Episodes=FALSE,verbose=FALSE) {
#	Episodes -> TRUE for representing episodes
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Plotting non-interpolated instantaneous heart rate **\n");
	}

   if (Episodes & is.null(Data$Episodes)) {
		cat("   --- ERROR: Episodes not present!! ---\n")
		return(invisible())
   }
	
	if (is.null(Data$Beat$Time)) { 
		cat("   --- ERROR: Beats not present!! ---\n")
		return(invisible())
	}
	
	if (is.null(Data$Beat$niHR)) { 
		cat("   --- ERROR: Non-interpolated heart rate not present!! ---\n")
		return(invisible())
	}
	
	if (verbose) {
		cat("   Number of points:",length(Data$Beat$Time),"\n");
	}
	
	plot(Data$Beat$Time,Data$Beat$niHR,type="l",xlab="time (sec.)",ylab="HR (beats/min.)")

   if (Episodes) {
      episodesLeft=Data$Episodes$InitTime
      episodesBottom=rep(min(Data$Beat$niHR),times=length(episodesLeft))
      episodesRight=Data$Episodes$InitTime+Data$Episodes$Duration
      episodesTop=rep(max(Data$Beat$niHR),times=length(episodesLeft))
      cat("   No of episodes:",length(episodesLeft),"\n")
      rect(episodesLeft,episodesBottom,episodesRight,episodesTop,border="red")
   }

	title(main="Non-interpolated instantaneous heart rate")
}

