AvgIntegralCorrelation <-
function(HRVData, Data, m, tau, r) {
  # -------------------------------------
  # Averages Integral Correlation
  # -------------------------------------
  .Deprecated("CalculateSampleEntropy")
	Cmr = IntegralCorrelation(HRVData,Data,m=m,tau=tau,r=r)
	
	VerboseMessage(HRVData$Verbose, 	
	               "Averaging Integral Correlation")
	
	Phi=log(sum(Cmr)/length(Cmr))
	
	VerboseMessage(HRVData$Verbose, 	
	               paste("Average Integral Correlation:", mean(Cmr)))
	
	return(Phi)
}

