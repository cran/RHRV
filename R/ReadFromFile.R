`ReadFromFile` <-
function(HRVData,name,verbose=FALSE) {
	nameext=sprintf("%s.%s",name,HRVData$Ext)
	
	if (verbose) {
		cat("** Reading file:",nameext,"\n")
	}
	
	if (!file.exists(nameext)) {
		stop("  --- File does not exist!! ---\n    --- Quitting now!! ---\n")
	}
	
	HRVData=dget(name)
	
	if (verbose) {
			cat("   ",file.info(nameext)$size," bytes read\n",sep="")
	}
}

