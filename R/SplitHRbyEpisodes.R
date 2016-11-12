SplitHRbyEpisodes <-
function(HRVData, Tag="", verbose=NULL) {
# -------------------------------------------------
# Splits Heart Rate Data using Episodes information
# -------------------------------------------------
#  Tag -> specifies tag of episodes
#  Returns a list with two vectors: InEpisodes and OutEpisodes

  HRVData = HandleVerboseArgument(HRVData, verbose)
  
  VerboseMessage(HRVData$Verbose, 
                 paste("Splitting heart rate signal using episodes"))
   

  CheckEpisodes(HRVData)
  CheckInterpolation(HRVData)
	VerboseMessage(HRVData$Verbose,
	               ifelse(Tag == "", "No tag was specified", 
	                      paste("Using episodes with tag:", Tag)))

   # Select episodes to split signal
	if (Tag == "") {
	  ActiveEpisodes = HRVData$Episodes
	} else {
	  ActiveEpisodes = subset(HRVData$Episodes,HRVData$Episodes$Type==Tag)
	}
	
	VerboseMessage(HRVData$Verbose, 
	               paste("Number of episodes:", length(ActiveEpisodes$InitTime)))
	
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
	
	VerboseMessage(HRVData$Verbose, 
	               paste("Inside episodes:", length(l$InEpisodes),"points"))
	VerboseMessage(HRVData$Verbose, 
	               paste("Outside episodes:",length(l$OutEpisodes),"points"))
	
	return(l)
}

