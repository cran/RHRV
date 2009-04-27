`CreateData` <-
function(verbose=FALSE) {
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Creating data model **\n")
	}
	
	Data <- list()
	Data$Beat=NULL
		# Beat annotations: dataframe containing:
		#	Time -> sequence of beats (seconds)
		#	niHR -> non interpolated instantaneous frequency (beats/min)
		
	Data$Episodes=NULL	
		# Episodes anotations: dataframe containing:
		#	InitTime -> init time for the episodes (seconds)
		#  	Type -> type of episodes
		#   Duration -> duration of episodes
		#   Value -> numerical value for episodes
		
	Data$Freq_param=NULL # Parameters frequency (default: 1 Hz)
	
	Data$Freq_HR=NULL # Heart Rate interpolation frequency (default: 4 Hz)
	
	Data$AnnotSep="#" # Character used as separator
	
	Data$Ext="hrv" # Extension for files
	
	Data$datetime=NULL
	
	
	if (verbose) {
		cat("   Data model created\n")
	}
	return(Data)
}

