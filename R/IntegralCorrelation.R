IntegralCorrelation <-
function(HRVData, Data, m, tau, r) {
# -------------------------------------
# Calculates Integral Correlation
# -------------------------------------
  .Deprecated("CalculateSampleEntropy")

	DataExp = BuildTakensVector(HRVData,Data,m=m,tau=tau)
	numelem = nrow(DataExp)

	VerboseMessage(HRVData$Verbose, "Calculating Integral Correlation")
	
	mutualDistance = as.matrix(dist(DataExp,method="maximum"))
	Cmr = array(1:numelem)
	
	
	for (i in 1:numelem) {
	  iDistance = mutualDistance[i, 1:numelem]
	  Cmr[i] = length(iDistance[iDistance <= r]) / numelem
	}
	
	VerboseMessage(HRVData$Verbose, paste("Integral Correlation:", rhrvFormat(sum(Cmr))))
	return(Cmr)
}

