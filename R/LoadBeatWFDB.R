LoadBeatWFDB <- function (HRVData, RecordName, RecordPath = ".", annotator = "qrs", verbose = NULL) {
#-------------------------------
# Loads beats from a WFDB file
#-------------------------------
#	RecordName -> record containing values 
#	RecordPath -> path
#	annotator -> type of file
#-------------------------------

	samplingFrequency = ""

	HRVData = HandleVerboseArgument(HRVData, verbose)
	
	VerboseMessage(HRVData$Verbose, 
	               paste("Loading beats positions for record:", RecordName))
	

	dir = getwd() 
	setwd(RecordPath)

	auxHeader = readLines(paste(RecordName,".hea",sep=""),1)
	splitAuxHeader = strsplit(auxHeader," ")

	if(length(splitAuxHeader[[1]])>2)
		samplingFrequency = splitAuxHeader[[1]][3]
	else
		samplingFrequency = "250"

	samplingFrequency = as.numeric(samplingFrequency)


	#Read binary file
	con = file(paste(RecordName,".",annotator,sep=""),"rb")

	counter=1
	acumulator=0
	beats=c()
	repeat
	{
		value = readBin(con,"integer",n=1,size=2,signed=FALSE)

		code = bitwShiftR(value,10)
		time = value %% 1024

		if(code==0 && time==0)
			break
		else
		{
			if(code==1)
			{
				acumulator=acumulator+time
				timeInSeconds = acumulator/samplingFrequency
				beats=c(beats,timeInSeconds)
				counter=counter+1
			}
			else
			{
				if(code==63)
				{
					jump = time%/%2 + time%%2
					for(i in 1:jump)
						value = readBin(con,"integer",n=1,size=2,signed=FALSE)
				}
				else
				{
					if(code==59 && time==0)
					{
					  tmp1 = readBin(con,"integer",n=1,size=2,signed=FALSE)
					  tmp2 = readBin(con,"integer",n=1,size=2,signed=FALSE)
					  time = tmp1*(2**16)+tmp2
					  acumulator=acumulator+time
					}
					else
						if(code!=60 && code!=61 && code!=62 && code!=22 && code!=0)
						{
							acumulator=acumulator+time
						}
				}
			}
		}
	}

	beats = unique(beats)
	HRVData$Beat = data.frame(Time = beats)
	HRVData = LoadHeaderWFDB(HRVData, RecordName, RecordPath=".")
	VerboseMessage(HRVData$Verbose, paste("Number of beats:", length(beats)))
	
	
	close(con)
	setwd(dir)
	return(HRVData)
}
