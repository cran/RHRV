IntegralCorrelation <-
function(HRVData, Data, m, tau, r) {
# -------------------------------------
# Calculates Integral Correlation
# -------------------------------------

	DataExp = BuildTakensVector(HRVData,Data,m=m,tau=tau)
	numelem = nrow(DataExp)

	if (HRVData$Verbose) {
		cat("** Calculating Integral Correlation **\n")
	}
	
	mutualDistance = as.matrix(dist(DataExp,method="maximum"))
	Cmr=array(1:numelem)

	for (i in 1:numelem){
		iDistance = mutualDistance[i,1:numelem]
		Cmr[i] = (length(iDistance[iDistance<=r])-1)/numelem
	}

	if (HRVData$Verbose) {
		cat("  Integral Correlation: ", sum(Cmr), "\n", sep="")
	}
	
	return(Cmr)
}

