CreateTimeAnalysis <-
  function(HRVData, size=300, numofbins=NULL, interval=7.8125, verbose=NULL ) {
    # ----------------------------------------------------
    # Creates a Time analysis associated to the data model
    # ----------------------------------------------------
    #  	size: size of window (sec.)
    #  	interval: width of bins in histogram for TINN and HRV index (msec.)
    HRVData = HandleVerboseArgument(HRVData, verbose)
    VerboseMessage(HRVData$Verbose, "Creating time analysis")
    
    num=length(HRVData$TimeAnalysis)
    HRVData$TimeAnalysis[[num+1]]=list()
    HRVData$TimeAnalysis[[num+1]]$size=size # length size for analysis
    
    # vecthist contains the bins for the histogram
    minRR=min(HRVData$Beat$RR)
    maxRR=max(HRVData$Beat$RR)
    if (!is.null(numofbins)){
      interval = (maxRR-minRR)/(numofbins-2)
      vecthist = seq(minRR-interval/2,maxRR+interval/2,len=numofbins+1)
    }else{
      medRR=(min(HRVData$Beat$RR)+max(HRVData$Beat$RR))/2.0
      lowhist=medRR-interval*ceiling((medRR-minRR)/interval)
      longhist=ceiling((maxRR-lowhist)/interval)+1
      vecthist=seq(from=lowhist,by=interval,length.out=longhist)
    }
    VerboseMessage(HRVData$Verbose, paste("Size of window:",size,"seconds"))
    VerboseMessage(HRVData$Verbose, 
                   paste("Width of bins in histogram:", interval,
                         "milliseconds"))
    
    # SDNN
    HRVData$TimeAnalysis[[num+1]]$SDNN=sd(HRVData$Beat$RR)
    
    WindowMin=head(HRVData$Beat$Time,n=1)
    WindowMax=WindowMin + size
    WindowIndex=1
    RRWindowMean=c(0)
    RRWindowSD=c(0)
    while (WindowMax < tail(HRVData$Beat$Time,1)) {
      RRWindow=HRVData$Beat$RR[HRVData$Beat$Time >= WindowMin & HRVData$Beat$Time < WindowMax]
      # check if there is an interval without beats
      if (length(RRWindow) == 0){
        message = paste(sep = "", "Interval without beats from ",WindowMin,
                        " to ",WindowMax," seconds! Returning NA in SDANN and SDNNIDX")
        warning(message)
        # introduce the NAs to ensure that the user notices the warning
        RRWindowMean[WindowIndex] = NA
        RRWindowSD[WindowIndex] = NA
        # there is no need to compute more windows
        break;
      }
      RRWindowMean[WindowIndex]=mean(RRWindow)
      RRWindowSD[WindowIndex]=sd(RRWindow)
      WindowMin = WindowMin+size
      WindowMax = WindowMax+size
      WindowIndex = WindowIndex+1
    }
    
    numberOfWindows = WindowIndex-1
    VerboseMessage(HRVData$Verbose, 
                   paste("Number of windows:",numberOfWindows))
  
    if (numberOfWindows <= 1){
      warning("There is no window or just one window. Cannot compute the standard deviation! Returning NA in SDANN")
    }
    # SDANN
    HRVData$TimeAnalysis[[num+1]]$SDANN=sd(RRWindowMean) 
    
    # SDNNIDX
    HRVData$TimeAnalysis[[num+1]]$SDNNIDX=mean(RRWindowSD) 
    
    # pNN50
    RRDiffs = diff(HRVData$Beat$RR)
    RRDiffs50=RRDiffs[abs(RRDiffs)>50]
    HRVData$TimeAnalysis[[num+1]]$pNN50=100.0*length(RRDiffs50)/length(RRDiffs)
    
    # SDSD
    HRVData$TimeAnalysis[[num+1]]$SDSD = sd(RRDiffs)
    
    # rMSSD
    HRVData$TimeAnalysis[[num+1]]$rMSSD=sqrt(mean(RRDiffs^2))
    
    # IRRR
    HRVData$TimeAnalysis[[num+1]]$IRRR=IQR(HRVData$Beat$RR)
    
    # MADRR
    HRVData$TimeAnalysis[[num+1]]$MADRR=median(abs(RRDiffs))
    
    # TINN and HRV index
    h = hist(HRVData$Beat$RR, breaks=vecthist, plot=FALSE)
    area=length(HRVData$Beat$RR)*interval
    maxhist=max(h$counts)
    HRVData$TimeAnalysis[[num+1]]$TINN=area/maxhist*2
    HRVData$TimeAnalysis[[num+1]]$HRVi=length(HRVData$Beat$RR)/maxhist
    
    
    
    VerboseMessage(
      HRVData$Verbose,
      c(
        paste("Data has now", num + 1, "time analyses\n"),
        paste(" SDNN:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$SDNN), "msec.\n"),
        paste(" SDANN:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$SDANN), "msec.\n"),
        paste(" SDNNIDX:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$SDNNIDX), "msec.\n"),
        paste(" pNN50:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$pNN50), "%\n"),
        paste(" SDSD:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$SDSD), "msec.\n"),
        paste(" r-MSSD:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$rMSSD), "msec.\n"),
        paste(" IRRR:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$IRRR), "msec.\n"),
        paste(" MADRR:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$MADRR), "msec.\n"),
        paste(" TINN:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$TINN), "msec.\n"),
        paste(" HRV index:", rhrvFormat(HRVData$TimeAnalysis[[num + 1]]$HRVi))
      )
    )    
    return(HRVData)
  }

