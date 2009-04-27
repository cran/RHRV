`LoadEpisodesAscii` <-
function(Data,FileName,datetime="1/1/1900 0:0:0", verbose=FALSE) {	
#	FileName -> file containing episodes
#	datetime -> date and time (DD/MM/YYYY HH:MM:SS)
#	Verbose -> TRUE for verbose mode

#  Example of file containing episodes:

#  Init_Time	Resp_Events	Durat	SaO2
#  00:33:00        GEN_HYPO	120.0	82.9
#  01:30:00        OBS_APNEA	60.0	81.0
#  ....

#  First line of file is discarded
#  Init_time is relative to the beginning of data
#  Duration in seconds
	
	if (verbose) {
		cat("** Loading episodes file:",FileName,"**\n")
	}
	x=read.table(FileName,skip=1)

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

	# calculating time in seconds, considering the initial time for the register
	time=strptime(x$V1,"%T")
	auxtime=strptime(paste(Data$datetime$hour,":",sep="",Data$datetime$min,":",Data$datetime$sec),"%T")
	time=difftime(time,auxtime, units="secs")

	Data$Episodes<-data.frame(InitTime=as.numeric(time),Type=x$V2,Duration=x$V3,Value=x$V4)
	
	if (verbose) {
		cat("   Loaded ",length(Data$Episodes$InitTime)," episodes from file\n",sep="")
	}

	return(Data)
}

