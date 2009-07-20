`LoadBeatWFDB` <-
function(HRVData,RecordName,RecordPath=".",annotator="qrs",verbose=FALSE) {
#------------------------------
# Loads beats from an wfdb file
#	Uses rdann from wfdbtools
#------------------------------
#	RecordName -> record containing beat positions
#	RecordPath -> path
#	annotator -> wfdb annotator parameter
#	Verbose -> TRUE for verbose mode

	if (verbose) {
		cat("** Loading beats positions for record:",RecordName,"**\n")
	}
	
	dir=getwd()
	if (verbose) {
		cat("   Path:",RecordPath,"\n")
	}
	setwd(RecordPath)
	
	# Calls rdann to read beat annotations
	command=paste("rdann -r ",RecordName," -a",annotator," -p \'N\' -x")
	if (verbose) {
		cat("   Command:",command,"\n")
	}
	x1=system(command,intern=TRUE)
	x2=substring(x1,1,9)
	beat=as.numeric(x2)
	if (verbose) {
		cat("   Number of beats:",length(beat),"\n")
	}
	
	HRVData$Beat=data.frame(Time=beat)

	if (is.null(HRVData$datetime)) {
      if (verbose) {
         cat("   Reading header info for:",RecordName,"\n")
      }
      HRVData = LoadHeaderWFDB(HRVData,RecordName,RecordPath,verbose)
   } else {
      if (verbose) {
         cat("   Header info already present for:",RecordName,"\n")
      }
   }
	
	setwd(dir)
		
	return(HRVData)
}

