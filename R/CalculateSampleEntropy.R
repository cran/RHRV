
############################## CalculateSampleEntropy ##########################
#' Sample Entropy (also known as Kolgomorov-Sinai Entropy)
#' @description
#' These functions measure the complexity of the RR time series. Large values of 
#' the Sample Entropy indicate high complexity whereas that smaller values characterize
#' more regular signals.
#' @details  The sample entropy is computed using:
#' \deqn{h_q(m,r) = log(C_q(m,r)/C_{q}(m+1,r))}{hq(m,r) = log(Cq(m,r)/Cq(m+1,r)),}
#' where \emph{m} is the embedding dimension and \emph{r} is the radius of the neighbourhood. When 
#' computing the correlation dimensions we use the linear regions from the correlation
#' sums in order to do the estimates. Similarly, the sample entropy \eqn{h_q(m,r)}{hq(m,r)} 
#' should not change for both various \emph{m} and \emph{r}.
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param indexNonLinearAnalysis Reference to the data structure that will contain the nonlinear analysis
#' @param doPlot Logical value. If TRUE (default), a plot of the correlation sum is shown
#' @return  The \emph{CalculateSampleEntropy} returns a HRVData structure containing the sample entropy computations of the 
#' RR time series under the \emph{NonLinearAnalysis} list.
#' @note In order to run this functions, it is necessary to have used the \emph{CalculateCorrDim} function. 
#' @references H. Kantz  and T. Schreiber: Nonlinear Time series Analysis (Cambridge university press)
#' @note This function is based on the \code{\link[nonlinearTseries]{sampleEntropy}} function from the 
#' nonlinearTseries package.
#' @examples
#' \dontrun{
#' # ...
#' hrv.data = CreateNonLinearAnalysis(hrv.data)
#' hrv.data = CalculateCorrDim(hrv.data,indexNonLinearAnalysis=1,minEmbeddingDim=2,
#'                             maxEmbeddingDim=8,timeLag=1,minRadius=1,maxRadius=15,
#'                             pointsRadius=20,theilerWindow=10,corrOrder=2,doPlot=FALSE)
#' hrv.data = CalculateSampleEntropy(hrv.data,indexNonLinearAnalysis=1,doPlot=FALSE)
#' PlotSampleEntropy(hrv.data,indexNonLinearAnalysis=1)
#' hrv.data = EstimateSampleEntropy(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(6,10))
#' }
#' @rdname CalculateSampleEntropy
#' @seealso \code{\link[nonlinearTseries]{sampleEntropy}} 
CalculateSampleEntropy <-
  function(HRVData, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis), doPlot = TRUE) {
    # -------------------------------------
    # Calculates sampleEntropy  
    # -------------------------------------
        
    ## some checkings    
    CheckAnalysisIndex(indexNonLinearAnalysis, length(HRVData$NonLinearAnalysis),"nonlinear")
    
    ## obtaining corrDim if possible
    CheckNonLinearComputations(
      HRVData, indexNonLinearAnalysis, "correlation",
      MissingNonLinearObjectMessage("Correlation object", "CalculateCorrDim()",
                                    "CalculateSampleEntropy()")
    )
    corrDimObject = HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$correlation$computations
    
    ## Computing sample entropy
    VerboseMessage(HRVData$Verbose,
                   paste("Computing the sample entropy of order",
                         nlOrder(corrDimObject)))
                  

    sampleEntropyObject = sampleEntropy(corrDimObject,do.plot=doPlot)
    HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$sampleEntropy$computations=sampleEntropyObject
      
    return(HRVData)
  }


############################## EstimateSampleEntropy ##########################
#' @param regressionRange Vector with 2 components denoting the range where the function will perform linear regression
#' @param useEmbeddings A numeric vector specifying which embedding dimensions should the algorithm use to compute
#' the sample entropy.
#' @return The \emph{EstimateSampleEntropy} function estimates the sample entropy of the 
#' RR time series  by performing a linear regression
#' over the radius' range specified in \emph{regressionRange}. If \emph{doPlot} is TRUE,
#' a graphic of the regression over the data is shown. In order to run \emph{EstimateSampleEntropy}, it
#' is necessary to have performed the sample entropy computations before with \emph{ComputeSampleEntropy}. The 
#' results are returned into the \emph{HRVData} structure, under the \emph{NonLinearAnalysis} list.
#' @rdname CalculateSampleEntropy
EstimateSampleEntropy <-
  function(HRVData, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis), regressionRange = NULL, 
           useEmbeddings = NULL, doPlot = TRUE) {
    # -------------------------------------
    # Estimates sample entropy
    # -------------------------------------
    
    CheckAnalysisIndex(indexNonLinearAnalysis, length(HRVData$NonLinearAnalysis),"nonlinear")
    
    ## obtaining sampleEntropy Object
    CheckNonLinearComputations(
      HRVData, indexNonLinearAnalysis, "sampleEntropy",
      MissingNonLinearObjectMessage("Sample Entropy calculations", 
                                    "CalculateSampleEntropy()",
                                    "EstimateSampleEntropy()")
    )
    
    sampleEntropyObject = HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$sampleEntropy$computations
    
    ## Estimating
    VerboseMessage(HRVData$Verbose,"Computing the sample entropy")
    
    sampleEntropyEstimate = estimate(sampleEntropyObject,
                                     regression.range = regressionRange,
                                     use.embeddings = useEmbeddings,
                                     do.plot = doPlot)
    HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$sampleEntropy$statistic = sampleEntropyEstimate
    
    VerboseMessage(HRVData$Verbose,
                   paste("Sample entropy=",
                         round(mean(sampleEntropyEstimate),4)))
    return(HRVData)
  }


############################## PlotSampleEntropy ##########################
#' @param ... Additional plot parameters.
#' @return  \emph{PlotSampleEntropy} shows a graphic of the sample entropy computations.
#' @rdname CalculateSampleEntropy
PlotSampleEntropy <-
  function(HRVData, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis), ...) {
    # -------------------------------------
    # Plot sample entropy
    # -------------------------------------
    
    CheckAnalysisIndex(indexNonLinearAnalysis, length(HRVData$NonLinearAnalysis),"nonlinear")
    
    ## obtaining sampleEntropy Object
    CheckNonLinearComputations(
      HRVData, indexNonLinearAnalysis, "sampleEntropy",
      MissingNonLinearObjectMessage("Sample Entropy calculations", 
                                    "CalculateSampleEntropy()",
                                    "PlotSampleEntropy()")
    )
    sampleEntropyObject = HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$sampleEntropy$computations
    
    plot(sampleEntropyObject, ...)
    par(mfrow = c(1,1))
    
  }


