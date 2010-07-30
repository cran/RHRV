ReadFromFile <-
function(HRVData, name, verbose=NULL) {
# ---------------------------
# Reads data model from a file
# ---------------------------

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	nameext=sprintf("%s.%s",name,HRVData$Ext)
	
	if (HRVData$Verbose) {
		cat("** Reading file:",nameext,"\n")
	}
	
	if (!file.exists(nameext)) {
		stop("  --- File does not exist!! ---\n    --- Quitting now!! ---\n")
	}
	
	HRVData=dget(name)
	
	if (HRVData$Verbose) {
			cat("   ",file.info(nameext)$size," bytes read\n",sep="")
	}
}

