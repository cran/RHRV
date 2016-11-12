LoadBeatPolar <- function(HRVData, RecordName, RecordPath=".", verbose = NULL) {
#-------------------------------
# Loads beats from an ascii file
#-------------------------------
#	RecordName -> record containing RR values
#	RecordPath -> path
#-------------------------------

	dir=getwd()
  on.exit(setwd(dir))

	
  HRVData = HandleVerboseArgument(HRVData, verbose)
  
  VerboseMessage(HRVData$Verbose,
                 paste("Loading beats positions for record:", RecordName))
  VerboseMessage(HRVData$Verbose, paste("Path:", RecordPath))
	

	setwd(RecordPath)

	# Extracts time and date information from file
	headerinfo=scan(RecordName,what=character(0),sep="=",skip=4,nlines=2,quiet=TRUE)	
	
	regexpdate="[0-9]{4}[0-9]{2}[0-9]{2}"
	if (length(headerinfo[regexpr(regexpdate,headerinfo)==1])) {
		dateinfo=headerinfo[regexpr(regexpdate,headerinfo)==1]
	} else {
		dateinfo="19000101"
	}
	
	regexptime="[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{1}"
	if (length(headerinfo[regexpr(regexptime,headerinfo)==1])) {
		timeinfo=headerinfo[regexpr(regexptime,headerinfo)==1]
	} else {
		timeinfo="00:00:00.0"
	}
	
	datetimeinfo = paste(dateinfo,timeinfo)
	datetimeaux = strptime(datetimeinfo,"%Y%m%d %H:%M:%S")
	
	VerboseMessage(HRVData$Verbose,
	               c(paste0("Date: ", sprintf("%02d", datetimeaux$mday),"/",
	                        sprintf("%02d", 1 + datetimeaux$mon),"/",
	                        1900 + datetimeaux$year, "\n"),
	                 paste0("Time: ",sprintf("%02d", datetimeaux$hour), ":",
	                        sprintf("%02d", datetimeaux$min), ":",
	                        sprintf("%02.01f", datetimeaux$sec))
	               )
	)
	HRVData$datetime = datetimeaux

	aux=scan(RecordName,what=character(0),sep="=",skip=39,quiet=TRUE)
	HRVData$Beat$RR=as.numeric(aux[-(1:which(aux=="[HRData]"))])

	HRVData$Beat$Time=cumsum(HRVData$Beat$RR)/1000
	VerboseMessage(HRVData$Verbose, 
	               paste("Number of beats:", length(HRVData$Beat$Time))
	)

  return(HRVData)
}
