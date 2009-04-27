`ReadFromFile` <-
function(Data,name,verbose=FALSE) {
	nameext=sprintf("%s.%s",name,Data$Ext)
	
	if (verbose) {
		cat("** Reading file:",nameext,"\n")
	}
	
	if (!file.exists(nameext)) {
		cat("--- ERROR: File does not exist!! ---\n")
		return(invisible())	
	}
	
	Data=dget(name)
	
	if (verbose) {
			cat("   ",file.info(nameext)$size," bytes read\n",sep="")
	}
}

