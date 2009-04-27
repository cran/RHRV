`PlotHR` <-
function(Data,Episodes=FALSE,verbose=FALSE) {
#	Episodes -> TRUE for representing episodes
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Plotting interpolated instantaneous heart rate **\n");
	}

   if (Episodes & is.null(Data$Episodes)) {
		cat("   --- ERROR: Episodes not present!! ---\n")
		return(invisible())
   }
	
	if (is.null(Data$HR)) { 
		cat("   --- ERROR: Interpolated heart rate not present!! ---\n")
		return(invisible())
	}
	
	npoints = length(Data$HR)
	if (verbose) {
		cat("   Number of points:",npoints,"\n");
	}

	
	first = head(Data$Beat$Time,1)
	last = tail(Data$Beat$Time,1)
	x=seq(first,last,length.out=npoints)
  	plot(x,Data$HR,type="l",xlab="time (sec.)",ylab="HR (beats/min.)")

   if (Episodes) {
      episodesLeft=Data$Episodes$InitTime
      episodesBottom=rep(min(Data$HR),times=length(episodesLeft))
      episodesRight=Data$Episodes$InitTime+Data$Episodes$Duration
      episodesTop=rep(max(Data$HR),times=length(episodesLeft))
      cat("   No of episodes:",length(episodesLeft),"\n")
      rect(episodesLeft,episodesBottom,episodesRight,episodesTop,border="red")
   }

	title(main="Interpolated instantaneous heart rate")
}

