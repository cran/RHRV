BuildTakensVector <-
function(HRVData, Data, m, tau) {
# -------------------------------------
# Calculates Takens expanded vectors
# -------------------------------------
  .Deprecated("BuildTakens")
	
  VerboseMessage(HRVData$Verbose, paste("Creating Takens expanded vectors"))
  VerboseMessage(HRVData$Verbose, paste("m:", m, "Tau:", tau))
  
  
  N = length(Data)
	jump = tau
	maxjump = (m-1)*jump
	jumpsvect = seq(0,maxjump,jump)
	numjumps = length(jumpsvect)
	numelem = N-maxjump
	DataExp = matrix(nrow=numelem,ncol=numjumps)
	
	for (i in 1:numelem) {
	  DataExp[i,1:numjumps] = Data[jumpsvect+i]
	}
	
	return(DataExp)
}


#' Build the Takens' vectors 
#' @description
#' This function builds the Takens' vectors of the Non Interpolated RR intervals.
#' The set  of Takens' vectors is the result of embedding the time series in
#'  a m-dimensional  space. That is, the \eqn{n^{th}} Takens' vector is defined as 
#' \deqn{T[n]=\{niRR[n], niRR[n+ timeLag],..., niRR[n+m*timeLag]\}.}
#' Taken's theorem states that we can then reconstruct an equivalent dynamical 
#' system to the original one (the 
#' dynamical system that generated the observed time series) by using the Takens' vectors.

#' @param HRVData Data structure that stores the beats register and information related to it
#' @param embeddingDim Integer denoting the dimension in which we shall embed the RR series.
#' @param timeLag Integer denoting the number of time steps that will be use to construct the 
#' Takens' vectors.
#' @return A matrix containing the Takens' vectors (one per row).
#' @note This function is based on the \code{\link[nonlinearTseries]{buildTakens}} function from the 
#' nonlinearTseries package.
#' @references H. Kantz  and T. Schreiber: Nonlinear Time series Analysis (Cambridge university press)
BuildTakens = function(HRVData, embeddingDim, timeLag){
  CheckNIHR(HRVData)
  buildTakens(HRVData$Beat$RR,embedding.dim = embeddingDim,time.lag = timeLag)
}
