`AnalyzeHRbyEpisodes` <-
function(HRVData,Tag="",func,verbose=FALSE) {
# ----------------------------------------------
# Analyzes Heart Rate using Episodes information
# ----------------------------------------------
#  Tag -> specifies tag of episodes
#  func -> function to apply 
#	Verbose -> TRUE for verbose mode
#  Returns a list with two objects result

#  Function func musts receive a vector and returns an object

	if (verbose) {
		cat("** Applying function to heart rate signal using episodic information **\n");
      cat("   Function: ",func,"()\n",sep="")
   }

   if (is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present\n    --- Quitting now!! ---\n")
   }

	if (is.null(HRVData$HR)) { 
      stop("  --- Interpolated heart rate not present\n    --- Quitting now!! ---\n")
	}

	if (verbose) {
      if (Tag=="") {
		   cat("   No tag was specified\n")
      } else {
		   cat("   Using episodes with tag:",Tag,"\n")
      }
	}

   vectors=SplitHRbyEpisodes(HRVData,Tag=Tag,verbose=FALSE)

   cadIn=sprintf("%s(vectors$InEpisodes)",func)
   cadOut=sprintf("%s(vectors$OutEpisodes)",func)
   
   result=list(resultIn=eval(parse(text=cadIn)),resultOut=eval(parse(text=cadOut)))

   return(result)



}

