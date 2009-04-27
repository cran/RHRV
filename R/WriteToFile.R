`WriteToFile` <-
function(Data,name,overwrite=TRUE,verbose=FALSE) {
	
	nameext=sprintf("%s.%s",name,Data$Ext)
	
	if (verbose) {
		cat("** Writing file:",nameext,"\n")
	}
	
	if (file.exists(nameext)) {
		if (verbose) {
			cat("   File ",nameext," already exists\n",sep="")
		}
		if (!overwrite) {
			cat("--- ERROR: File exists... no overwriting it!! ---\n")
			return(invisible())
		}
	}
		
	dput(Data,file=nameext)
	if (verbose) {
			cat("   ",file.info(nameext)$size," bytes written\n",sep="")
	}
}

