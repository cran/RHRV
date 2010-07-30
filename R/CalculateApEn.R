CalculateApEn <-
function(HRVData, indexNonLinearAnalysis = -1, m=2, tau=1, r=0.2, N=1000, verbose=NULL) {
# -------------------------------------
# Calculates Approximate Entropy
# -------------------------------------

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	npoints = length(HRVData$HR)

	if (npoints > N) {
		DataInt=HRVData$HR[(npoints/2-N/2):(npoints/2+N/2)] 
	}
	else{
		DataInt=HRVData$HR
	}
	r = r*sd(DataInt)
	
	Phi1 = AvgIntegralCorrelation(HRVData,DataInt,m=m,tau=tau,r=r)
	Phi2 = AvgIntegralCorrelation(HRVData,DataInt,m=(m+1),tau=tau,r=r)

	if (HRVData$Verbose) {
		cat("** Calculating Approximate Entropy **\n")
	}

	ApEn = Phi1-Phi2

	if (HRVData$Verbose) {
		cat("  Approximate Entropy: ", ApEn, "\n", sep="")
	}
	
    HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$ApEn=ApEn
    
    return(HRVData)
}

