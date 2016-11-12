LoadApneaWFDB <-
  function(HRVData, RecordName, RecordPath=".", Tag="APNEA", verbose=NULL) {
    #--------------------------------------- 
    # Loads apnea episodes from an wfdb file
    #---------------------------------------
    #	RecordName -> record containing beat positions
    #	RecordPath -> path
    #  Tag -> tag to include in episodes
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    
    
    VerboseMessage(HRVData$Verbose,
                   paste("Loading apnea episodes for record:", RecordName))
    
    
    dir=getwd()
    on.exit(setwd(dir))
    
    VerboseMessage(HRVData$Verbose, paste("Path:", RecordPath))
    
    setwd(RecordPath)
    
    # Reads header, verbose=FALSE
    if (is.null(HRVData$datetime)) {
      VerboseMessage(HRVData$Verbose, 
                     paste("Reading header info for:", RecordName))
      HRVData = LoadHeaderWFDB(HRVData,RecordName,RecordPath)
    } else {
      VerboseMessage(HRVData$Verbose,  
                     paste("Header info already present for:",RecordName))
    }
    
    auxHeader = readLines(paste(RecordName,".hea",sep=""),1)
    splitAuxHeader = strsplit(auxHeader," ")
    
    if(length(splitAuxHeader[[1]])>2)
      samplingFrequency = splitAuxHeader[[1]][3]
    else
      samplingFrequency = "250"
    
    samplingFrequency = as.numeric(samplingFrequency)
    
    VerboseMessage(HRVData$Verbose, 
                   paste("Sampling frequency for apnea annotations:",
                         samplingFrequency))
    
    inApnea = FALSE
    accumulator = 0
    initT = c()
    endT = c()
    con = file(paste(RecordName,".apn",sep=""),"rb")
    repeat {
      value = readBin(con,"integer",n=1,size=1,signed=FALSE)+256*readBin(con,"integer",n=1,size=1,signed=FALSE)
      
      #message(paste("value:",value))
      
      code = bitwShiftR(value,10)
      #message(paste("code:",code))
      
      time = value %% 1024
      
      #message(paste("time:",time))
      
      if(code==0 && time==0)
        break
      
      if (code==8 && !inApnea) {
        #message(paste("Onset: ", accumulator))
        inApnea = TRUE
        if (accumulator > 30)
          initT = c(initT,accumulator-30)
        else
          initT = c(initT, accumulator)
      }
      
      if (code==1 && inApnea) {
        #message(paste("End: ",accumulator))
        inApnea = FALSE
        endT = c(endT,accumulator-30)
      }
      
      if (code==59) {
        interval = (readBin(con,"integer",n=1,size=1,signed=FALSE)+readBin(con,"integer",n=1,size=1,signed=FALSE)*256)*65536+(readBin(con,"integer",n=1,size=1,signed=FALSE)+readBin(con,"integer",n=1,size=1,signed=FALSE)*256)
        accumulator = accumulator + interval/samplingFrequency
        next
      }
      
    }
    
    if (inApnea) {
      endT = c(endT,accumulator)
      #message(paste("End: ",accumulator))
    }
    
    close(con)
    
    if (length(initT) > 0) { 
      HRVData = AddEpisodes(
        HRVData,
        InitTimes = initT,
        Tags = Tag,
        Durations = endT-initT,
        Values = 0
      ) 
    }
    return(HRVData)
  }

