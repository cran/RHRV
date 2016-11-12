AddEpisodes <-
  function(HRVData, InitTimes,Tags, Durations ,Values, verbose=NULL) {	
    #------------------
    # Adds new episodes 
    #------------------
    #	InitTimes -> Vector containing initial times in seconds
    #	Tags -> Vector containing types of episodes
    #	Durations -> Vector containing durations in seconds
    #	Values -> Vector containing numerical values for episodes
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    
    VerboseMessage(HRVData$Verbose, "Adding new episodes")
    
    NewEpisodes=data.frame(InitTime=InitTimes,Type=Tags,Duration=Durations,Value=Values)
    VerboseMessage(HRVData$Verbose, 
                   paste("Added", length(NewEpisodes$InitTime), "episodes from file"))
    
    HRVData$Episodes = rbind(HRVData$Episodes, NewEpisodes)
    HRVData$Episodes = HRVData$Episodes[order(HRVData$Episodes$InitTime), ]  # Sort episodes by InitTime
    HRVData$Episodes = HRVData$Episodes[!duplicated(HRVData$Episodes), ]  # Remove duplicated episodes
    
    VerboseMessage(HRVData$Verbose, 
                   paste("Number of episodes:", length(HRVData$Episodes$InitTime)))
    
    
    return(HRVData)
  }

