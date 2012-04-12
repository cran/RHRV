AvgIntegralCorrelation <-
function(HRVData, Data, m, tau, r) {
# -------------------------------------
# Averages Integral Correlation
# -------------------------------------

	Cmr = IntegralCorrelation(HRVData,Data,m=m,tau=tau,r=r)

	if (HRVData$Verbose) {
		cat("** Averaging Integral Correlation **\n")
	}

	Phi=sum(log(Cmr))/length(Cmr)

	if (HRVData$Verbose) {
		cat("  Average Integral Correlation: ", Phi, "\n", sep="")
	}
	
	return(Phi)
}

