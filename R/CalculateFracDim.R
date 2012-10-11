CalculateFracDim <-
function(HRVData, indexNonLinearAnalysis = -1, m=10, tau=3, Cra=0.005, Crb=0.75, N=1000, verbose=NULL) {
# -------------------------------------
# Calculates Fractal Dimension
# -------------------------------------

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	npoints = length(HRVData$Beat$niHR)

	if (npoints > N) {
		DataInt=HRVData$Beat$niHR[(npoints/2-N/2):(npoints/2+N/2)] 
	}
	else{
		DataInt=HRVData$Beat$niHR
	}
	
	randC = CalculateRfromCorrelation(HRVData, DataInt, m=m, tau=tau, Cra=Cra, Crb=Crb)
	ra = randC[1,1]
	rb = randC[1,2]
	Cmra = randC[2,1]
	Cmrb = randC[2,2]
		
	if (HRVData$Verbose) {
		cat("** Calculating Fractal Dimension **\n")
	}

	FracDim = (log(Cmrb)-log(Cmra))/(log(rb)-log(ra))

	if (HRVData$Verbose) {
		cat("  Fractal Dimension: ", FracDim, "\n", sep="")
	}
	
    HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$FracDim=FracDim
    
    return(HRVData)
}

