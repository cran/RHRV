`LoadBeatAscii` <-
function(Data,FileName,scale=1,datetime="1/1/1900 0:0:0",verbose=FALSE) {
#	FileName -> file containing beat positions
#	scale -> 1 if positions in seconds
#	datetime -> date and time (DD/MM/YYYY HH:MM:SS)
#	Verbose -> TRUE for verbose mode


	if (verbose) {
		cat("** Loading file:",FileName,"**\n")
		cat("   Scale:",scale,"\n");
	}
	x=read.table(FileName)
	
	# obtaining date of the record
	datetimeaux = strptime(datetime,"%d/%m/%Y %H:%M:%S")
	if (is.na(datetimeaux)) {
		cat("   --- ERROR: Date/time format is dd/mm/yyyy HH:MM:SS ---\n")
		return(Data)
	}	
	if (verbose) {
		cat("   Date: ",sprintf("%02d",datetimeaux$mday),"/",
			sprintf("%02d",1+datetimeaux$mon),"/",
			1900+datetimeaux$year,"\n",sep="")
		cat("   Time: ",sprintf("%02d",datetimeaux$hour),":",
			sprintf("%02d",datetimeaux$min),":",
			sprintf("%02d",datetimeaux$sec),"\n",sep="")
	}
	Data$datetime=datetimeaux
	
	# applying scale and sampling frequency	
	Data$Beat=data.frame(Time=x$V1*scale)
	
	if (verbose) {
		cat("   Number of beats:",length(Data$Beat$Time),"\n")
	}
		
	return(Data)
}

