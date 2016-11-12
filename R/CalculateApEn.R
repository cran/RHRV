CalculateApEn <-
function(HRVData, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis), m=2, tau=1, r=0.2, N=1000, verbose=NULL) {
# -------------------------------------
# Calculates Approximate Entropy
# -------------------------------------
	.Deprecated("CalculateSampleEntropy")
  HRVData = HandleVerboseArgument(HRVData, verbose)
  
	npoints = length(HRVData$Beat$niHR)

	if (npoints > N) {
	  DataInt = HRVData$Beat$niHR[(npoints / 2 - N / 2):(npoints / 2 + N / 2)] 
	}
	else{
	  DataInt = HRVData$Beat$niHR
	}
	r = r*sd(DataInt)
	
	VerboseMessage(HRVData$Verbose, "Calculating Approximate Entropy")
	Phi1 = AvgIntegralCorrelation(HRVData,DataInt,m = m,tau = tau,r = r)
	Phi2 = AvgIntegralCorrelation(HRVData,DataInt,m = (m + 1),tau = tau,r = r)
	ApEn = Phi1 - Phi2
	VerboseMessage(HRVData$Verbose, paste("Approximate Entropy: ", ApEn))
	
	HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$ApEn=ApEn
	return(HRVData)
}

