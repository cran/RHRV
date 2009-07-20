`WriteToFile` <-
function(HRVData,name,overwrite=TRUE,verbose=FALSE) {
# ---------------------------
# Writes data model to a file
# ---------------------------
#	overwrite: if true, overwrites previously existing file
	
	nameext=sprintf("%s.%s",name,HRVData$Ext)
	
	if (verbose) {
		cat("** Writing file:",nameext,"\n")
	}
	
	if (file.exists(nameext)) {
		if (verbose) {
			cat("   File ",nameext," already exists\n",sep="")
		}
		if (!overwrite) {
			stop("  --- File exists... No overwriting it!! ---\n    --- Quitting now!! ---\n")
		}
	}
		
	dput(HRVData,file=nameext)
	if (verbose) {
			cat("   ",file.info(nameext)$size," bytes written\n",sep="")
	}
}

