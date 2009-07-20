`SplitHRbyEpisodes` <-
function(HRVData,Tag="",verbose=FALSE) {
# -------------------------------------------------
# Splits Heart Rate Data using Episodes information
# -------------------------------------------------
#  Tag -> specifies tag of episodes
#  Verbose -> TRUE for verbose mode
#  Returns a list with two vectors: InEpisodes and OutEpisodes

	if (verbose) {
		cat("** Splitting heart rate signal using episodes **\n");
   }

   if (is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present\n    --- Quitting now!! ---\n")
   }

	if (is.null(HRVData$HR)) { 
      stop("  --- Interpolated heart rate not present\n    --- Quitting now!! ---\n")
	}

	if (verbose) {
      if (Tag=="") {
		   cat("   No tag was specified\n");
      } else {
		   cat("   Using episodes with tag:",Tag,"\n");
      }
	}

   # Select episodes to split signal
   if (Tag=="") {
      ActiveEpisodes=HRVData$Episodes
   } else {
      ActiveEpisodes=subset(HRVData$Episodes,HRVData$Episodes$Type==Tag)
   }

   if (verbose) {
      cat("   Number of episodes:",length(ActiveEpisodes$InitTime),"\n")
   }

   Beg=ActiveEpisodes$InitTime
   End=ActiveEpisodes$InitTime+ActiveEpisodes$Duration

	npoints = length(HRVData$HR)
	first = head(HRVData$Beat$Time,1)
	last = tail(HRVData$Beat$Time,1)
	x=seq(first,last,length.out=npoints)

   # Auxiliary signal used to mark points inside episodes
   Aux=rep(0,times=npoints)
   for (i in 1:length(Beg)) {
      Aux[x>=Beg[i] & x<=End[i]] = 1
   }

   l=list(InEpisodes=HRVData$HR[Aux==1],OutEpisodes=HRVData$HR[Aux==0])

   if (verbose) {
      cat("   Inside episodes:",length(l$InEpisodes),"points\n")
      cat("   Outside episodes:",length(l$OutEpisodes),"points\n")
   }

   return(l)
}

