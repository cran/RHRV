LoadBeatSuunto <- function(HRVData, RecordName, RecordPath=".", verbose = NULL) {
#-------------------------------
# Loads beats from an ascii file
#-------------------------------
#	RecordName -> record containing RR values
#	RecordPath -> path
#-------------------------------

	dir=getwd()

	HRVData = HandleVerboseArgument(HRVData, verbose)
	VerboseMessage(HRVData$Verbose,
	               paste("Loading beats positions for record:", RecordName))
	VerboseMessage(HRVData$Verbose, paste("Path:", RecordPath))

	setwd(RecordPath)

	#Date and time information
	aux=scan(RecordName,what=character(0),sep="=",quiet=TRUE)
	date=aux[which(aux=="STARTTIME")+1]
	dateAux = substr(date,1,10)
	dateAux = gsub("\\.","-",dateAux)

	time = substr(date,12,19)
	time = gsub("\\.",":",time)
		
		
	VerboseMessage(HRVData$Verbose, paste("Date: ",dateAux))
	VerboseMessage(HRVData$Verbose, paste("Time: ",time))
		
	datetimeinfo = paste(dateAux,time,sep = " ")
	HRVData$datetime=datetimeinfo

	aux=scan(RecordName,what=character(0),sep="=",quiet=TRUE)
	HRVData$Beat$RR=as.numeric(aux[-(1:which(aux=="[CUSTOM1]"))])

	HRVData$Beat$Time=cumsum(HRVData$Beat$RR)/1000
	VerboseMessage(HRVData$Verbose,
	               paste("Number of beats:", length(HRVData$Beat$Time)))
    	
	setwd(dir)
	return(HRVData)
}
