CalculateRfromCorrelation <-
function(HRVData, Data, m, tau, Cra, Crb) {
# -------------------------------------
# Calculates ra and rb from Correlation
# -------------------------------------
  .Deprecated("CalculateCorrDim")
	randC = matrix(nrow=2, ncol=2)
	
	DataExp = BuildTakensVector(HRVData,Data,m=m,tau=tau)
	#	numelem = nrow(DataExp)
	
	VerboseMessage(HRVData$Verbose, "Calculating R from Correlation")
	
	
	mutualDistance = dist(DataExp,method="maximum")
	
	numelem = length(mutualDistance)
	rs = quantile(mutualDistance, probs = c(0.005, 0.75))
	ra = rs[1]
	rb = rs[2]
	
	Cmra = length(mutualDistance[mutualDistance <= ra]) / numelem
	Cmrb = length(mutualDistance[mutualDistance <= rb]) / numelem
	
	VerboseMessage(HRVData$Verbose, paste("ra =", rhrvFormat(ra)))
	VerboseMessage(HRVData$Verbose, paste("rb =", rhrvFormat(rb)))


	VerboseMessage(HRVData$Verbose, paste("Cmra =", rhrvFormat(Cmra * 100)))
	VerboseMessage(HRVData$Verbose, paste("Cmrb =", rhrvFormat(Cmrb * 100)))

	randC[1,1] = ra
	randC[1,2] = rb
	randC[2,1] = Cmra
	randC[2,2] = Cmrb
	
	return(randC)
}

