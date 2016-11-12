GenerateEpisodes <-
  function(HRVData, NewBegFrom = c("Beg", "End"), NewEndFrom = c("End", "Beg"),
           DispBeg, DispEnd, OldTag="", NewTag="", verbose=NULL) {	
    #-----------------------------------
    # Creates new episodes from old ones
    #-----------------------------------
    #  	NewBegFrom, NewEndFrom -> source of new beginning and end of episodes ("Beg" for indicating the same beginning as that of the old episode, "End" for end)
    #  	DispBeg, DispEnd -> absolute displacements for new episodes in seconds
    #  	OldTag -> specifies tag of old episodes
    #  	NewTag -> specifies tag for new episodes (if empty, copies OldTag)
    
    #  	Example: arguments for creating episodes one minute before old ones
    #    	NewBegFrom="Beg", NewEndFrom="End", DispBeg=-60, DispEnd=-60
    
    #  	Example: arguments for creating episodes just after previous ones (length = 1 min.) 
    #    	NewBegFrom="End", NewEndFrom="End", DispBeg=0, DispEnd=60
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    NewBegFrom = match.arg(NewBegFrom)
    NewEndFrom = match.arg(NewEndFrom)
    
    VerboseMessage(HRVData$Verbose,
                   c("Creating new episodes from old ones\n",
                     ifelse(OldTag == "", 
                            "No tag specified: using all old episodes\n",
                            paste("Using episodes with tag:", OldTag, "\n")),
                     ifelse(NewTag == "",
                            "Duplicating old tags",
                            paste("Creating episodes with tag:", NewTag)))
    )
    
    if (OldTag == "") {
      NewEpisodes = HRVData$Episodes
    } else {
      NewEpisodes = subset(HRVData$Episodes, HRVData$Episodes$Type == OldTag)
    }
    
    if (NewBegFrom == "Beg") {
      BegInstants = NewEpisodes$InitTime + DispBeg
    } else {
      BegInstants = NewEpisodes$InitTime + NewEpisodes$Duration + DispBeg
    }
    
    if (NewEndFrom == "Beg") {
      EndInstants = NewEpisodes$InitTime + DispEnd
    } else {
      EndInstants = NewEpisodes$InitTime + NewEpisodes$Duration + DispEnd
    } 
    
    NewEpisodes$InitTime = BegInstants
    NewEpisodes$Duration = EndInstants - BegInstants
    if (NewTag != "") {
      NewEpisodes$Type = NewTag
    }
    
    VerboseMessage(HRVData$Verbose, 
                   paste("Created", length(NewEpisodes$InitTime),
                         "episodes from previous ones")
    )
    
    HRVData$Episodes = rbind(HRVData$Episodes, NewEpisodes)
    HRVData$Episodes = HRVData$Episodes[order(HRVData$Episodes$InitTime), ]  # Sorts episodes by InitTime
    HRVData$Episodes = HRVData$Episodes[!duplicated(HRVData$Episodes), ]  # Removes duplicated episodes
    
    VerboseMessage(HRVData$Verbose, 
                   paste("Number of episodes:", length(HRVData$Episodes$InitTime))
    )
    
    return(HRVData)
  }

