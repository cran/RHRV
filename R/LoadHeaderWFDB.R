`LoadHeaderWFDB` <-
function(Data,RecordName,RecordPath=".",verbose=FALSE) {
#	RecordName -> record containing beat positions
#	RecordPath -> path
#	Verbose -> TRUE for verbose mode

	dir=getwd()
	if (verbose) {
		cat("   Path:",RecordPath,"\n")
	}
	setwd(RecordPath)

	# Extracts time and date information from wfdb header
	headerfile=paste(RecordName,".hea",sep="")
	if (verbose) {
		cat("   Opening header file:",headerfile,"\n")
	}
	headerinfo=scan(headerfile,what=character(0),nlines=1,quiet=TRUE)	
	
	regexptime="[[:digit:]]{2}:[[:digit:]]{2}:[[:digit:]]{2}"
	if (length(headerinfo[regexpr(regexptime,headerinfo)==1])) {
		timeinfo=headerinfo[regexpr(regexptime,headerinfo)==1]
		if (verbose) {
			cat("      Time information in header:",timeinfo,"\n")
		}
	} else {
		if (verbose) {
			timeinfo="00:00:00"
			cat("      No time information in header:",timeinfo,"\n")
		}
	}
	
	regexpdate="[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}"
	if (length(headerinfo[regexpr(regexpdate,headerinfo)==1])) {
		dateinfo=headerinfo[regexpr(regexpdate,headerinfo)==1]
		if (verbose) {
			cat("      Date information in header:",dateinfo,"\n")
		}
	} else {
		if (verbose) {
			dateinfo="01/01/1900"
			cat("      No date information in header:",dateinfo,"\n")
		}
	}
	
	datetimeinfo = paste(dateinfo,timeinfo)
	datetimeaux = strptime(datetimeinfo,"%d/%m/%Y %H:%M:%S")
	
	if (verbose) {
		cat("   Date: ",sprintf("%02d",datetimeaux$mday),"/",
			sprintf("%02d",1+datetimeaux$mon),"/",
			1900+datetimeaux$year,"\n",sep="")
		cat("   Time: ",sprintf("%02d",datetimeaux$hour),":",
			sprintf("%02d",datetimeaux$min),":",
			sprintf("%02d",datetimeaux$sec),"\n",sep="")
	}
	Data$datetime=datetimeaux

	setwd(dir)

	return(Data)

}

