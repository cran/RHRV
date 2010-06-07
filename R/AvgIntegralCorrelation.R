AvgIntegralCorrelation <-
function(HRVData, Data, m, tau, r) {
# -------------------------------------
# Averages Integral Correlation
# -------------------------------------

	Cmr = IntegralCorrelation(HRVData,Data,m=m,tau=tau,r=r)

	if (HRVData$Verbose) {
		cat("** Averaging Integral Correlation **\n")
	}

	Phi=sum(log(Cmr[Cmr!=0]))/length(Cmr[Cmr!=0])

	if (HRVData$Verbose) {
		cat("  Average Integral Correlation: ", Phi, "\n", sep="")
	}
	
	return(Phi)
}

