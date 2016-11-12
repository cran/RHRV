ListEpisodes <-
  function(HRVData, TimeHMS = FALSE) {
    # ----------------------------------------
    # Lists episodes included in a RHRV record
    # ----------------------------------------
    
    CheckEpisodes(HRVData)
    
    charsep <- "     "
    
    Index = sprintf("%3d",seq(length(HRVData$Episodes$InitTime)))
    Tag = paste(charsep,HRVData$Episodes$Type)
    if (TimeHMS) {
      numhours <- floor(HRVData$Episodes$InitTime/3600)
      nummins <- floor((HRVData$Episodes$InitTime - numhours*3600)/60)
      numsecs <- HRVData$Episodes$InitTime - numhours*3600 - nummins*60
      InitTime <- ifelse(numhours!=0,
                         sprintf("%s%2dh %02dm %04.1fs", charsep, numhours, nummins, numsecs),
                         ifelse(nummins!=0,
                                sprintf("%s%2dm %04.1fs", charsep, nummins, numsecs),
                                sprintf("%s%4.1fs", charsep, numsecs)
                         )
                         
      )
      
      numhours <- floor(HRVData$Episodes$Duration/3600)
      nummins <- floor((HRVData$Episodes$Duration - numhours*3600)/60)
      numsecs <- HRVData$Episodes$Duration - numhours*3600 - nummins*60
      Duration <- ifelse(numhours!=0,
                         sprintf("%s%2dh %02dm %04.1fs", charsep, numhours, nummins, numsecs),
                         ifelse(nummins!=0,
                                sprintf("%s%2dm %04.1fs", charsep, nummins, numsecs),
                                sprintf("%s%4.1fs", charsep, numsecs)
                         )
      )
      
    } else {
      InitTime <- paste(charsep,HRVData$Episodes$InitTime)
      Duration <- paste(charsep,HRVData$Episodes$Duration)
    }
    
    
    Value <- paste(charsep,HRVData$Episodes$Value)
    dat <- data.frame(Index,Tag,InitTime,Duration,Value)
    print.data.frame(dat,row.names=FALSE)
  }