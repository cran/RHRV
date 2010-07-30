LoadBeatWFDB <-
function(HRVData, RecordName, RecordPath=".", annotator="qrs", verbose=NULL) {
#------------------------------
# Loads beats from an wfdb file
#	Uses rdann from wfdbtools
#------------------------------
#	RecordName -> record containing beat positions
#	RecordPath -> path
#	annotator -> wfdb annotator parameter

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Loading beats positions for record:",RecordName,"**\n")
	}
	
	dir=getwd()
	if (HRVData$Verbose) {
		cat("   Path:",RecordPath,"\n")
	}
	setwd(RecordPath)
	
	# Calls rdann to read beat annotations
	command=paste("rdann -r ",RecordName," -a",annotator," -p \'N\' -x")
	if (HRVData$Verbose) {
		cat("   Command:",command,"\n")
	}
	x1=system(command,intern=TRUE)
	x2=substring(x1,1,9)
	beat=as.numeric(x2)
	if (HRVData$Verbose) {
		cat("   Number of beats:",length(beat),"\n")
	}
	
	HRVData$Beat=data.frame(Time=beat)

	if (is.null(HRVData$datetime)) {
      if (HRVData$Verbose) {
         cat("   Reading header info for:",RecordName,"\n")
      }
      HRVData = LoadHeaderWFDB(HRVData,RecordName,RecordPath)
   } else {
      if (HRVData$Verbose) {
         cat("   Header info already present for:",RecordName,"\n")
      }
   }
	
	setwd(dir)
		
	return(HRVData)
}

