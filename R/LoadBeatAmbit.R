LoadBeatAmbit <- function(HRVData, RecordName, RecordPath=".", verbose = NULL) {
#-------------------------------
# Loads beats from a Suunto Ambit XML file
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
	aux=scan(RecordName,what=character(0),strip.white=TRUE,quiet=TRUE)
	date=aux[grepl('DateTime',aux)]
	dateAux=substr(date,11,20)

    time <- substr(date,22,29)
		
		
	VerboseMessage(HRVData$Verbose, paste("Date: ", dateAux))
	VerboseMessage(HRVData$Verbose, paste("Time: ", time))
	
	datetimeinfo = paste(dateAux,time,sep = " ")
	HRVData$datetime=datetimeinfo

	BeatPosition=grep('IBI',aux)
    rawIBI=aux[BeatPosition[1]:BeatPosition[2]]
    rawIBI[1]=gsub('<IBI>','',rawIBI[1])
    rawIBI[length(rawIBI)]=gsub('</IBI>','',rawIBI[length(rawIBI)])
	HRVData$Beat$RR=as.numeric(rawIBI)

	HRVData$Beat$Time=cumsum(HRVData$Beat$RR)/1000
	VerboseMessage(HRVData$Verbose, 
	               paste("Number of beats:", length(HRVData$Beat$Time))
	)
	setwd(dir)
	return(HRVData)
}
