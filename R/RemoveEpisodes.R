RemoveEpisodes <-
  function(HRVData, Tags=NULL, Indexes=NULL) {	
    #-----------------------------
    # Removes Episodes from record
    #-----------------------------
    #	Tags -> Vector containing types of episodes
    #	Indexes -> Vector containing indexes of episodes
    
    
    
    VerboseMessage(HRVData$Verbose, paste("Removing episodes"))
    
    
    noEpBefore <- length(HRVData$Episodes$InitTime)
    VerboseMessage(HRVData$Verbose, 
                   paste("Number of episodes before removal:", noEpBefore))
    
    
    HRVData$Episodes <- selectEpisodes(HRVData$Episodes,Tags,Indexes)
    HRVData$Episodes <- HRVData$Episodes[!HRVData$Episodes$selected,]  # Removal happens here
    HRVData$Episodes$selected <- NULL
    
    HRVData$Episodes <- HRVData$Episodes[order(HRVData$Episodes$InitTime),]  # Sort episodes by InitTime
    
    
    noEpAfter <- length(HRVData$Episodes$InitTime)
    VerboseMessage(HRVData$Verbose, 
                   paste("Number of episodes after removal: ", noEpAfter))
    
    
    VerboseMessage(HRVData$Verbose,
                   ifelse(noEpAfter == noEpBefore, 
                          "No episode was removed",
                          paste("Number of episodes removed:", noEpBefore - noEpAfter))
    )		
    
    
    if (length(HRVData$Episodes$InitTime) == 0) {
      HRVData$Episodes <- NULL
      VerboseMessage(HRVData$Verbose, "All episodes were removed from data")
    }
    
    return(HRVData)
  }

