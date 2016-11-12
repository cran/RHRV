ReadFromFile <-
function(name, verbose=FALSE) {
# ---------------------------
# Reads data model from a file
# ---------------------------

  HRVData = CreateHRVData(Verbose = verbose)
  nameext = sprintf("%s.%s", name, HRVData$Ext)
  
	VerboseMessage(HRVData$Verbose, paste("Reading file:",nameext))
	if (!file.exists(nameext)) {
		stop("File does not exist!")
	}
	HRVData = dget(nameext)
	VerboseMessage(HRVData$Verbose,
	               paste(file.info(nameext)$size," bytes read"))
	
  return(HRVData)
}

