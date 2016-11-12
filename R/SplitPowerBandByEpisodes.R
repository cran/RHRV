SplitPowerBandByEpisodes <-
  function(HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis), Tag="",
           verbose=NULL) {
    # ------------------------------------------------
    # Splits Power Per Band using Episodes information
    # ------------------------------------------------
    #  Tag -> specifies tag of episodes
    #  Returns a list with two lists: InEpisodes and OutEpisodes
    #    Both lists include ULF, VLF, LF and HF bands
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    CheckEpisodes(HRVData)
    CheckPowerBand(HRVData, indexFreqAnalysis)
    VerboseMessage(HRVData$Verbose, "Splitting power bands using episodes")
    VerboseMessage(HRVData$Verbose,
                   ifelse(Tag == "", 
                          "No tag was specified", 
                          paste("Using episodes with tag:", Tag))
    )
    
    # Select episodes to split bands
    if (Tag == "") {
      ActiveEpisodes = HRVData$Episodes
    } else {
      ActiveEpisodes = subset(HRVData$Episodes, HRVData$Episodes$Type == Tag)
    }
    
    VerboseMessage(HRVData$Verbose, 
                   paste("Number of episodes:", length(ActiveEpisodes$InitTime)))
    
    lframes=length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV)
    # lframes is the number of frames
    
    # message("No. of frames: ", lframes)
    # message("Beginning of file: ",head(HRVData$Beat$Time,1))
    # message("End of file: ",tail(HRVData$Beat$Time,1))
    
    indexInEp=c()
    indexOutEp=c()
    
    if (HRVData$FreqAnalysis[[indexFreqAnalysis]]$type=="fourier"){
      useShift <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$shift
      useSize <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$size
    }else{
      useShift <- useSize <- 1 / HRVData$Freq_HR
    }
    
    for (i in 1:lframes) {
      BegOfFrame = head(HRVData$Beat$Time,1)+useShift*(i-1)
      EndOfFrame = BegOfFrame + useSize
      # if (i<10 || i>lframes-10) {
      #    message("Frame: ",i,"  -  ")
      #    message("CenterOfFrame: ",CenterOfFrame)
      # }
      inEp = FALSE
      outEp = TRUE
      if (length(ActiveEpisodes$InitTime)>0) {
        for (j in 1:length(ActiveEpisodes$InitTime)) {
          begEp = ActiveEpisodes$InitTime[j]
          endEp = ActiveEpisodes$InitTime[j]+ActiveEpisodes$Duration[j]
          if (BegOfFrame>=begEp && EndOfFrame<=endEp) {
            inEp = TRUE
          }
          if (BegOfFrame>=begEp && BegOfFrame<=endEp) {
            outEp = FALSE
          }
          if (EndOfFrame>=begEp && EndOfFrame<=endEp) {
            outEp = FALSE
          }
          
        }
      }
      if (inEp) {
        indexInEp=c(indexInEp,i)
      }
      
      if (outEp) {
        indexOutEp=c(indexOutEp,i)
      }
      
    } # for (i in 1:lframes)
    
    if (length(indexInEp)==0){
      warning(paste0("no frames in episodes with tag '",Tag,"'!"))
    }
    
    if (length(indexOutEp)==0){
      warning(paste0("no frames outside episodes with tag '",Tag,"'!"))
    }
    
    l=list()
    
    
    l$InEpisodes=list(ULF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[indexInEp],
                      VLF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[indexInEp],
                      LF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[indexInEp],
                      HF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[indexInEp]
    )
    
    
    l$OutEpisodes=list(ULF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[indexOutEp],
                       VLF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[indexOutEp],
                       LF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[indexOutEp],
                       HF=HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[indexOutEp]
    )   
    
    VerboseMessage(
      HRVData$Verbose, 
      c(paste0("No. of frames: ", lframes, "\n"),
        paste0("No. of frames in episodes: ", length(l$InEpisodes$ULF), "\n"),
        paste0("No. of frames outside episodes: ", length(l$OutEpisodes$ULF), "\n"),
        paste0("No. of borderline frames: ", lframes - length(l$InEpisodes$ULF) - length(l$OutEpisodes$ULF)))
    )
    
    return(l)
    
  }

