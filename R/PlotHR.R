`PlotHR` <-
function(HRVData,Tag=NULL,verbose=FALSE) {
# -----------------------------
# Plots interpolated Heart Rate
# -----------------------------
#	Tag -> Tags of episodes to include in the plot
#    "all" includes all types
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Plotting interpolated instantaneous heart rate **\n");
	}
   
   if (!is.null(Tag) & is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present ---\n    --- Quitting now!! ---\n")
   }

	if (is.null(HRVData$HR)) { 
      stop("  --- Interpolated heart rate not present ---\n    --- Quitting now!! ---\n")
	}
	
	npoints = length(HRVData$HR)
	if (verbose) {
		cat("   Number of points:",npoints,"\n");
	}

	
	first = head(HRVData$Beat$Time,1)
	last = tail(HRVData$Beat$Time,1)
	x=seq(first,last,length.out=npoints)
  	plot(x,HRVData$HR,type="l",xlab="time (sec.)",ylab="HR (beats/min.)")

   if (!is.null(Tag)) {

      if (Tag[1]=="all") {
         Tag=levels(HRVData$Episodes$Type)
      }

      if (verbose) {
         cat("   Episodes in plot:",Tag,"\n")
      }

      # Data for representing episodes
      EpisodesAuxLeft=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag]
      EpisodesAuxBottom=rep(min(HRVData$HR),times=length(EpisodesAuxLeft))
      EpisodesAuxRight=HRVData$Episodes$InitTime[HRVData$Episodes$Type %in% Tag] + 
         HRVData$Episodes$Duration[HRVData$Episodes$Type %in% Tag]
      EpisodesAuxTop=rep(max(HRVData$HR),times=length(EpisodesAuxLeft))
      EpisodesAuxType=HRVData$Episodes$Type[HRVData$Episodes$Type %in% Tag]

      Pal=rainbow(length(Tag))
      Bor=Pal[match(EpisodesAuxType,Tag)]

      cat("   No of episodes:",length(EpisodesAuxLeft),"\n")
      cat("   No of classes of episodes:",length(Pal),"\n")
      rect(EpisodesAuxLeft,EpisodesAuxBottom,EpisodesAuxRight,EpisodesAuxTop,border=Bor)
      legend("bottom",legend=Tag,fill=Pal,cex=0.6,ncol=length(Tag),bty="n",inset=-0.01)
   }

	title(main="Interpolated instantaneous heart rate")
}

