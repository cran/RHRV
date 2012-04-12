LoadBeat <- function(fileType, HRVData, Recordname, Recordpath = ".", annotator = "qrs", scale = 1, datetime = "1/1/1900 0:0:0", annotationType = "QRS", verbose = NULL) {
#-------------------------------
# Loads beats from a specific file
#-------------------------------
#	fileType -> type of the file
#	RecordName -> record containing values
#	RecordPath -> path
#-------------------------------

	toret=""
	if(fileType == "WFDB")
	{
		toret = LoadBeatWFDB(HRVData,Recordname,Recordpath,annotator,verbose)
	}
	else
	{
		if(fileType == "Ascii")
		{
			toret = LoadBeatAscii(HRVData, Recordname, Recordpath, scale, datetime, verbose)
		}
		else
		{
			if(fileType == "RR")
			{
				toret = LoadBeatRR(HRVData, Recordname, Recordpath, scale, datetime, verbose)
			}	
			else
		 	{
				if(fileType == "Polar")
				{
					toret = LoadBeatPolar(HRVData, Recordname, Recordpath, verbose)
				}
				else
				{
					if(fileType == "Suunto")
					{
						toret = LoadBeatSuunto(HRVData, Recordname, Recordpath, verbose)
					}
					else
					{
						if(fileType == "EDFPlus")
						{
							toret = LoadBeatEDFPlus(HRVData, Recordname, Recordpath, annotationType, verbose)
						}
						else
						{
							print("Error: unknown file type")
						}
					}
				}
			}
		}
	}
	return(toret)
}
