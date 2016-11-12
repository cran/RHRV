WriteToFile <-
function(HRVData, name, overwrite=TRUE, verbose=NULL) {
# ---------------------------
# Writes data model to a file
# ---------------------------
#	overwrite: if true, overwrites previously existing file
	
  HRVData = HandleVerboseArgument(HRVData, verbose)
	
	nameext=sprintf("%s.%s",name,HRVData$Ext)
	
	VerboseMessage(HRVData$Verbose, paste("Writing file:",nameext))
	
	
	if (file.exists(nameext)) {
	  VerboseMessage(HRVData$Verbose, paste("File", nameext, "already exists"))
	  if (!overwrite) {
	    stop("File exists... Not overwriting it!")
		}
	}
		
	dput(HRVData,file=nameext)
	VerboseMessage(HRVData$Verbose, 
	               paste(file.info(nameext)$size,"bytes written"))
	
}

