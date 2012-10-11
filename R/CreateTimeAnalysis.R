CreateTimeAnalysis <-
function(HRVData, size=300, numofbins=20, interval=7.8125, verbose=NULL ) {
# ----------------------------------------------------
# Creates a Time analysis associated to the data model
# ----------------------------------------------------
#  	size: size of window (sec.)
#  	interval: width of bins in histogram for TINN and HRV index (msec.)

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
   	if (HRVData$Verbose) {
      	cat("** Creating time analysis\n")
   	}

   	num=length(HRVData$TimeAnalysis)

   	HRVData$TimeAnalysis[[num+1]]=list()

   	HRVData$TimeAnalysis[[num+1]]$size=size # length size for analysis

   	if (HRVData$Verbose) {
      	cat("   Size of window:",size,"seconds \n")
      	cat("   Width of bins in histogram:",interval,"milliseconds \n")
   	}

   	# SDNN
   	HRVData$TimeAnalysis[[num+1]]$SDNN=sd(HRVData$Beat$RR)

   	WindowMin=0.0
   	WindowMax=size
   	WindowIndex=1
   	RRWindowMean=c(0)
   	RRWindowSD=c(0)
   	while (WindowMax < tail(HRVData$Beat$Time,1)) {
      	RRWindow=HRVData$Beat$RR[HRVData$Beat$Time > WindowMin & HRVData$Beat$Time < WindowMax]
      	RRWindowMean[WindowIndex]=mean(RRWindow)
      	RRWindowSD[WindowIndex]=sd(RRWindow)
      	WindowMin = WindowMin+size
      	WindowMax = WindowMax+size
      	WindowIndex = WindowIndex+1
   	}

   	if (HRVData$Verbose) {
      	cat("   Number of windows:",WindowIndex-1,"\n")
   	}

   	# SDANN
   	HRVData$TimeAnalysis[[num+1]]$SDANN=sd(RRWindowMean) 

   	# SDNNIDX
   	HRVData$TimeAnalysis[[num+1]]$SDNNIDX=mean(RRWindowSD) 

   	# pNN50
	NRRs=length(HRVData$Beat$RR)
   	RRDiffs = HRVData$Beat$RR[2:NRRs]-HRVData$Beat$RR[1:NRRs-1]
   	RRDiffs50=RRDiffs[abs(RRDiffs)>50]
   	HRVData$TimeAnalysis[[num+1]]$pNN50=100.0*length(RRDiffs50)/length(RRDiffs)

   	# rMSSD
   	HRVData$TimeAnalysis[[num+1]]$rMSSD=sqrt(mean(RRDiffs^2))

   	# IRRR
   	RRQuant=quantile(RRDiffs)
   	HRVData$TimeAnalysis[[num+1]]$IRRR=RRQuant[[4]]-RRQuant[[2]]

   	# MADRR
   	HRVData$TimeAnalysis[[num+1]]$MADRR=median(abs(RRDiffs))

   	# TINN and HRV index
   	# vecthist contains the bins for the histogram
   	minRR=min(HRVData$Beat$RR)
   	maxRR=max(HRVData$Beat$RR)
   	medRR=(min(HRVData$Beat$RR)+max(HRVData$Beat$RR))/2.0
   	lowhist=medRR-interval*ceiling((medRR-minRR)/interval)
   	longhist=ceiling((maxRR-lowhist)/interval)+1
   	vecthist=seq(from=lowhist,by=interval,length.out=longhist)

   	h = hist(HRVData$Beat$RR, breaks=vecthist, plot=FALSE)
   	area=length(HRVData$Beat$RR)*interval
   	maxhist=max(h$counts)
   	HRVData$TimeAnalysis[[num+1]]$TINN=area/maxhist
   	HRVData$TimeAnalysis[[num+1]]$HRVi=length(HRVData$Beat$RR)/maxhist



   	if (HRVData$Verbose) {
      	cat("   Data has now",num+1,"time analyses\n")
      	cat("      SDNN:",HRVData$TimeAnalysis[[num+1]]$SDNN,"msec. \n")
      	cat("      SDANN:",HRVData$TimeAnalysis[[num+1]]$SDANN,"msec. \n")
     	cat("      SDNNIDX:",HRVData$TimeAnalysis[[num+1]]$SDNNIDX,"msec. \n")
      	cat("      pNN50:",HRVData$TimeAnalysis[[num+1]]$pNN50,"%\n")
      	cat("      r-MSSD:",HRVData$TimeAnalysis[[num+1]]$rMSSD,"msec.\n")
      	cat("      IRRR:",HRVData$TimeAnalysis[[num+1]]$IRRR,"msec.\n")
      	cat("      MADRR:",HRVData$TimeAnalysis[[num+1]]$MADRR,"msec.\n")
      	cat("      TINN:",HRVData$TimeAnalysis[[num+1]]$TINN,"msec.\n")
      	cat("      HRV index:",HRVData$TimeAnalysis[[num+1]]$HRVi,"\n")
   	}

   	return(HRVData)
}

