LoadHeaderWFDB <-
function(HRVData, RecordName, RecordPath=".", verbose=NULL) {
#------------------------------------
# Loads header info from an wfdb file
#------------------------------------
#	RecordName -> record containing beat positions
#	RecordPath -> path

  HRVData = HandleVerboseArgument(HRVData, verbose)
  
	dir=getwd()
  on.exit(setwd(dir))

  VerboseMessage(HRVData$Verbose, paste("Path:", RecordPath))
	
	setwd(RecordPath)

	# Extracts time and date information from wfdb header
	headerfile=paste(RecordName,".hea",sep="")
  VerboseMessage(HRVData$Verbose, paste("Opening header file:", headerfile))
	headerinfo=scan(headerfile,what=character(0),nlines=1,quiet=TRUE)	
	
	regexptime="[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}"
	if (length(headerinfo[regexpr(regexptime,headerinfo)==1])) {
	  timeinfo=headerinfo[regexpr(regexptime,headerinfo)==1]
	  VerboseMessage(HRVData$Verbose, 
	                 paste("Time information in header:",timeinfo))
	} else {
	  timeinfo="00:00:00"
	  VerboseMessage(HRVData$Verbose, 
	                 paste("No time information in header:", timeinfo))
	}
	
	regexpdate="[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}"
	if (length(headerinfo[regexpr(regexpdate,headerinfo)==1])) {
	  dateinfo=headerinfo[regexpr(regexpdate,headerinfo)==1]
	  VerboseMessage(HRVData$Verbose, 	
	                 paste("Date information in header:",dateinfo))
	} else {
	  dateinfo="01/01/1900"
	  VerboseMessage(HRVData$Verbose, 
	                 paste("No date information in header:", dateinfo))
	}
	
	datetimeinfo = paste(dateinfo,timeinfo)
	datetimeaux = strptime(datetimeinfo,"%d/%m/%Y %H:%M:%S")
	
	VerboseMessage(HRVData$Verbose, 
	               paste0("Date: ", sprintf("%02d",datetimeaux$mday), "/",
	                      sprintf("%02d",1 + datetimeaux$mon), "/",
	                      1900 + datetimeaux$year))
	VerboseMessage(HRVData$Verbose, 	
	               paste0("Time: ", sprintf("%02d",datetimeaux$hour),":",
	                      sprintf("%02d",datetimeaux$min),":",
	                      sprintf("%02d",datetimeaux$sec)))
  
	HRVData$datetime=datetimeaux

	return(HRVData)

}

