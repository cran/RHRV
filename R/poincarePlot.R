############################# Poincare plot ####################################
#' Poincare Plot
#' @description  The Poincare plot is a graphical representation of the dependance
#'  between successive RR intervals obtained by plotting the \eqn{RR_{j+\tau}}{RR_(j+tau)}
#'  as a function of \eqn{RR_j}. This dependance is often quantified by fitting an
#'  ellipse to the plot. In this way, two parameters are obtained:  
#' \eqn{SD_1}  and \eqn{SD_2}.
#' \eqn{SD_1} characterizes short-term variability
#' whereas that \eqn{SD_2} characterizes long-term variability.
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param indexNonLinearAnalysis Reference to the data structure that will contain the nonlinear analysis
#' @param timeLag Integer denoting the number of time steps that will be use to construct the 
#' dependance relation:  \eqn{RR_{j+timeLag}}{RR_(j+timeLag)} as a function of \eqn{RR_j}.
#' @param confidenceEstimation Logical value. If TRUE, the covariance matrix is
#' used for fitting the ellipse and computing the \eqn{SD_1} and
#' \eqn{SD_2} parameters (see details). Default: FALSE. 
#' @param confidence The confidence used for plotting the confidence ellipse.
#' @param doPlot Logical value. If TRUE (default), the PoincarePlot is shown. 
#' @param xlab A title for the x axis.
#' @param ylab A title for the y axis.
#' @param main An overall title for the Poincare plot.
#' @param pch Plotting character (symbol to use).
#' @param cex Character (or symbol) expansion.     
#' @param type What type of plot should be drawn. See \code{\link[graphics]{plot.default}}.
#' @param xlim x coordinates range. If not specified, a proper x range is selected.
#' @param ylim y coordinates range. If not specified, a proper y range is selected.
#' @param ... Additional parameters for the Poincare plot figure.
#' @details In the HRV literature, when \emph{timeLag = 1}, the \eqn{SD_1} and \eqn{SD_2}
#' parameters are computed using time domain measures. This is the default approach in this
#' function if \emph{timeLag=1}. This function also allows the user to fit a ellipse
#'  by computing the covariance matrix of 
#'  (\eqn{RR_{j}}{RR_(j)},\eqn{RR_{j+\tau}}{RR_(j+tau)})
#' (by setting \emph{confidenceEstimation = TRUE}). In most cases, both approaches
#' yield similar results.
#' @examples
#' \dontrun{
#'  data(HRVProcessedData)
#'  # rename for convenience
#'  hd = HRVProcessedData
#'  hd = CreateNonLinearAnalysis(hd)
#'  hd = PoincarePlot(hd, doPlot = T)
#' }
#' @return  A \emph{HRVData} structure containing a \emph{PoincarePlot} field storing
#' the \eqn{SD_1} and \eqn{SD_2} parameters. The \emph{PoincarePlot} field is
#' stored under the \emph{NonLinearAnalysis} list.
PoincarePlot = function(HRVData, indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
                        timeLag = 1, confidenceEstimation = FALSE, confidence = 0.95,
                        doPlot =FALSE, main = "Poincare plot", xlab="RR[n]", 
                        ylab = paste0("RR[n+",timeLag,"]"), pch=1,
                        cex=0.3, type = "p", xlim = NULL, ylim = NULL,
                        ...){
  # -------------------------------------
  # Poincare plot and SD1 and SD2 index
  # -------------------------------------
  CheckAnalysisIndex(indexNonLinearAnalysis, length(HRVData$NonLinearAnalysis), 
                     "nonlinear")
  
  VerboseMessage(HRVData$Verbose, "Calculating SD1 and SD2 parameters")  
  CheckNIHR(HRVData)
  
  if ( (confidence < 0) || (confidence > 1) ) {
    stop("Confidence must be in the [0,1] interval.")
  }
  
  rrSeries = HRVData$Beat$RR
  SD = computeSD(timeSeries = rrSeries, timeLag = timeLag,
                 confidenceEstimation = confidenceEstimation,
                 confidence = confidence)
  # Get critical value depending on the confidenceValue
  cv2 = qchisq(confidence, 2)
  cv = sqrt(cv2)  
  # greatest value is SD2
  sd1 = SD$sd[[2]]
  sd2 = SD$sd[[1]]
  sd1Direction = SD$direction[,2]
  sd2Direction = SD$direction[,1]
  # plot if necessary
  if (doPlot) {
    VerboseMessage(HRVData$Verbose,   
                   paste("Creating Poincare Plot with time lag = ",timeLag))
    # get 2D-phase space
    takens = buildTakens(rrSeries, embedding.dim = 2, time.lag = timeLag)
    mu = c(mean(takens[,1]), mean(takens[,2]))
    # Compute the ellipse sorrounding the points: a will denote the largest 
    # axis of the ellipse and b the shortest one.
    ellipse  = getEllipse(a = sd2 * cv, b = sd1 * cv, 
                          aVector = sd2Direction, bVector = sd1Direction,
                          mu = mu)
    # Compute end-points for the SD1 and SD2 arrows 
    SD1_end = mu + sd2 * cv * sd2Direction
    SD2_end = mu + sd1 * cv * sd1Direction
    # compute xlim and ylim if necessary
    if (is.null(xlim)) {
      xlim = range(c(takens[,1], ellipse[,1], SD1_end[[1]], SD2_end[[1]]))
    }
    if (is.null(ylim)) {
      ylim = range(c(takens[,2], ellipse[,2], SD1_end[[2]], SD2_end[[2]]))
    }
    plot(takens[,1],takens[,2], type = type, col = 2, pch = pch, cex = cex,
         xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = ylim,
         ...)
    # Plot the ellipse
    lines(ellipse, col = "black", lwd = 5)  
    # plot SD1 
    arrows(x0 = mu[[1]], y0 = mu[[2]], x1 = SD1_end[[1]], y1 = SD1_end[[2]],
           angle = 15, col = 3, code = 2, lty = 1, lwd = 2)
    # plot SD2
    arrows(x0 = mu[[1]], y0 = mu[[2]], x1 = SD2_end[[1]], y1 = SD2_end[[2]],
           angle = 15, col = 4, code = 2, lty = 1, lwd = 2)
    legend("bottomright", c("SD1","SD2"),  bty = "n",
           col = c(3,4), lty = c(1,1), lwd = c(2,2))
  }
  
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PoincarePlot$SD1 = sd1
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PoincarePlot$SD2 = sd2
  
  VerboseMessage(HRVData$Verbose, paste("SD1 = ", rhrvFormat(sd1)))
  VerboseMessage(HRVData$Verbose, paste("SD2 = ", rhrvFormat(sd2)))
  
  return(HRVData)
}

computeSD <- function(timeSeries, timeLag, confidenceEstimation, confidence){
  # compute parameters
  if (confidenceEstimation) {
    takens = buildTakens(time.series = timeSeries,
                         embedding.dim = 2,
                         time.lag = timeLag)
    SD = confidenceEllipse(x = takens[, 1],
                           y = takens[, 2],
                           confidence = confidence)
  }else{
    sd1 = sd(diff(timeSeries)) / sqrt(2)
    sd2 = sqrt(2 * var(timeSeries) - sd1 ^ 2)  
    directions = matrix(c(1, 1, -1, 1) / sqrt(2), 2, byrow = FALSE)
    # return values in decreasing order
    SD = list(sd = c(sd2, sd1), directions = directions)
  }
  return(SD)
}

getEllipse <- function(a, aVector, b, bVector, mu){
  angle = seq(0, 2*pi, len = 100)
  # Get the ellipse
  xEllipse = a * cos(angle)
  yEllipse = b * sin(angle)
  ellipse = cbind(xEllipse, yEllipse)
  # Rotate the ellipse and return
  mu + t(cbind(aVector,bVector) %*% t(ellipse))
}


confidenceEllipse <- function(x,y, confidence=0.95){
  # Get covariance matrix and mean point
  covMatrix = cov(cbind(x,y))
  # Compute eigenvalues and eigenvectors
  eigenComputations = eigen(covMatrix)
  eigenvalues = eigenComputations$values
  eigenvectors = eigenComputations$vectors
  # rotate eigenvectors to obtain "usual" directions
  # first eigenvector must have all components > 0
  if ( (eigenvectors[1,1]*eigenvectors[1,2]) < 0) {
    rotationMatrix = matrix(c(-1,0,0,-1), ncol = 2,
                            byrow = FALSE)
    eigenvectors = rotationMatrix %*% eigenvectors
  }
  
  return(list(sd = sqrt(eigenvalues), directions = eigenvectors))
}
