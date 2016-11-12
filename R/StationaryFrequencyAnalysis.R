############################## CalculatePSD ##########################
#' Spectral Density Estimation
#' @description Estimate the Power Spectral Density (PSD) of the RR time series.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param indexFreqAnalysis An integer referencing the data structure that will contain 
#' the frequency analysis.
#' @param method String specifying the method used to estimate the spectral 
#' density. Allowed methods are "pgram" (the default), "ar" and "lomb". 
#' @param doPlot Plot the periodogram?
#' @param ... Further arguments to specific PSD estimation methods or 
#' \code{\link[RHRV]{PlotPSD}}.
#' @details The "pgram" and "ar" methods use the \code{\link[stats]{spec.pgram}}
#' and \code{\link[stats]{spec.ar}} functions. Thus, the same arguments used
#' in  \code{\link[stats]{spec.pgram}} or \code{\link[stats]{spec.ar}} 
#' can be used when method is "pgram" or "ar", respectively.
#' The "lomb"  is based in the \code{\link[lomb]{lsp}} and thus it accepts the
#' same parameters as this function.
#' @return  The \emph{CalculatePSD} returns the \emph{HRVData} structure 
#' containing a \emph{periodogram} field storing and PSD estimation 
#' of the RR time series. When the "pgram" and "ar" methods are used the
#' \emph{periodogram} field is an object of class "spec". If "lomb" is used,
#' the \emph{periodogram} field is just a list. In any case the 
#' \emph{periodogram} field will contain:
#' \itemize{
#'  \item{freq: vector of frequencies at which the spectral density is estimated.}
#'  \item{spec: spectral density estimation }
#'  \item{series: name of the series}
#'  \item{method: method used to calculate the spectrum}
#' }
#' @examples
#' \dontrun{
#' data(HRVData)
#' HRVData=BuildNIHR(HRVData)
#' HRVData=FilterNIHR(HRVData)
#' # Frequency analysis requires interpolated data (except Lomb)
#' HRVData=InterpolateNIHR(HRVData)
#' # Create a different freqAnalysis for each method
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,1,"pgram",doPlot = F)
#' 
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,2,"pgram",spans=9, doPlot = F)
#' 
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,3,"ar",doPlot = F)
#' 
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,4,"lomb",doPlot = F)
#' # Plot the results
#' layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
#' PlotPSD(HRVData,1)
#' PlotPSD(HRVData,2)
#' PlotPSD(HRVData,3)
#' PlotPSD(HRVData,4)
#' }
#' @rdname CalculatePSD
#' @seealso \code{\link[stats]{spectrum}}, \code{\link[RHRV]{PlotPSD}}.
CalculatePSD =
  function(HRVData, indexFreqAnalysis = length(HRVData$FreqAnalysis),
           method=c("pgram","ar","lomb"), doPlot=T, ...){
    
    CheckAnalysisIndex(indexFreqAnalysis, length(HRVData$FreqAnalysis),
                       "frequency")
    
    switch(match.arg(method),
           pgram = spec.pgramAdapter(HRVData,indexFreqAnalysis,doPlot,...),
           ar = spec.arAdapter(HRVData,indexFreqAnalysis,doPlot,...),
           lomb = lombAdapter(HRVData,indexFreqAnalysis,doPlot,...))
  }
# Private Function for CalculatePSD. Performs periodogram 
# estimation using AR modelling. It is just an adapter of the
# spec.ar function from the stats package
spec.arAdapter = function(HRVData, indexFreqAnalysis, doPlot = TRUE,
                          n.freq, order = NULL,
                          na.action = na.fail,  method = "yule-walker",
                          ...){
  CheckInterpolation(HRVData)
  
  VerboseMessage(HRVData$Verbose,  "Calculating Periodogram using AR modelling")
  
  # Compute the spectrogram using RR data
  x = ts(1000.0/(HRVData$HR/60.0),frequency = HRVData$Freq_HR,
         start=HRVData$Beat$Time[[1]])
  periodogram = stats::spec.ar(x,n.freq,order,plot=F,na.action, method)
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram=periodogram
  if (doPlot){
    PlotPSD(HRVData,indexFreqAnalysis,...)
  }
  HRVData
}

# Private Function for CalculatePSD. Performs periodogram 
# estimation using Daniell smoothers. It is just an adapter of the
# spec.pgram function from the stats package
spec.pgramAdapter = function(HRVData, indexFreqAnalysis,doPlot = TRUE,
                             spans = NULL, kernel=NULL, taper = 0.1,
                             pad = 0, fast = TRUE, demean = FALSE, detrend = TRUE,
                             na.action = na.fail, ...){
  
  CheckInterpolation(HRVData)
  
  VerboseMessage(HRVData$Verbose, 
                 "Calculating Periodogram using DFT + Daniell smoothers")
  
  # Compute the spectrogram using RR data
  x = ts(1000.0/(HRVData$HR/60.0),frequency = HRVData$Freq_HR,
         start=HRVData$Beat$Time[[1]])
  periodogram = stats::spec.pgram(x, spans, kernel, taper,
                                  pad, fast, demean, detrend,
                                  plot = FALSE, na.action)
  
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram=periodogram
  if (doPlot){
    PlotPSD(HRVData,indexFreqAnalysis,...)
  }
  HRVData
  
}

lombAdapter = function(HRVData,indexFreqAnalysis,doPlot=TRUE,
                       from = 0, to = 0.5, type = c("frequency", "period"),
                       ofac = 1, alpha = 0.01,...){
  kCopyNames = c("n","type","ofac","n.out", "alpha",
                 "peak.at","p.value")  
  
  CheckNIHR(HRVData)
  
  VerboseMessage(HRVData$Verbose, 
                 "Calculating Periodogram using Lomb periodogram")
  
  # Compute the spectrogram using RR data
  lombper = lomb::lsp(x=HRVData$Beat$RR,times=HRVData$Beat$Time,
                    from, to, type,
                    ofac, alpha, plot=FALSE, ...)
  periodogram = lombper[kCopyNames]
  periodogram$series = "rr"
  periodogram$method = "Lomb Periodogram"
  periodogram$freq = lombper$scanned
  # renormalize spectrum
  var.RR = var(HRVData$Beat$RR)
  periodogram$spec = lombper$power * var.RR
  periodogram$sig.level = lombper$sig.level * var.RR
  periodogram$peak = lombper$peak * var.RR
  HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram=periodogram
  if (doPlot){
    PlotPSD(HRVData,indexFreqAnalysis,...)
  }
  HRVData

}


#' Plot Spectral Density Estimation
#' @description Plot the PSD estimate of the RR time series distinguishing
#' the different frequency bands with different colurs.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param indexFreqAnalysis An integer referencing the data structure that contains 
#' the PSD analysis.
#' @param ULFmin Lower limit ULF band used for distinguish the ULF band.
#' @param ULFmax Upper limit ULF band used for distinguish the ULF band.
#' @param VLFmin Lower limit VLF band.
#' @param VLFmax Upper limit VLF band.
#' @param LFmin	 Lower limit LF band.
#' @param LFmax	 Upper limit LF band.
#' @param HFmin	 Lower limit HF band.
#' @param HFmax	 Upper limit HF band.
#' @param type 1-character string giving the type of plot desired. See \code{\link[graphics]{plot.default}}.
#' @param xlim the x limits (x1, x2) of the plot. See \code{\link[graphics]{plot.default}}.
#' @param ylim the y limits of the plot.
#' @param log	 a character string which contains "x" if the x axis is to be
#' logarithmic,  "y" if the y axis is to be logarithmic and "xy" or "yx" if 
#' both axes are to be logarithmic. Default: "y".
#' @param main a main title for the plot. See \code{\link[graphics]{plot.default}}.
#' @param xlab a label for the x axis. See \code{\link[graphics]{plot.default}}.
#' @param ylab a label for the y axis. See \code{\link[graphics]{plot.default}}.
#' @param addLegend add a simple legend? Default: True.
#' @param addSigLevel Logical value (only used with the lomb method). If true an
#' horizontal line limiting the significance level is included ( Powers >
#'  sig.level can be considered significant peaks). See \code{\link[lomb]{lsp}}.
#' @param usePalette A new palette of colors for plotting the frequency bands. 
#' @param ... graphical parameters. See \code{\link[graphics]{plot.default}}. 
#' @examples
#' \dontrun{
#' data(HRVData)
#' HRVData=BuildNIHR(HRVData)
#' HRVData=FilterNIHR(HRVData)
#' # Frequency analysis requires interpolated data (except Lomb)
#' HRVData=InterpolateNIHR(HRVData)
#' # Create a different freqAnalysis for each method
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,1,"pgram",doPlot = F)
#' 
#' HRVData=CalculatePSD(HRVData,2,"pgram",spans=9,doPlot = F)
#' 
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,3,"ar",doPlot = F)
#' 
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,4,"lomb",doPlot = F)
#' # Plot the results
#' layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
#' PlotPSD(HRVData,1)
#' PlotPSD(HRVData,2)
#' PlotPSD(HRVData,3)
#' PlotPSD(HRVData,4)
#' }
#' @rdname PlotPSD
#' @seealso \code{\link[stats]{spectrum}}, \code{\link[lomb]{lsp}}, \code{\link[RHRV]{CalculatePSD}}.
#' 
PlotPSD = function(HRVData, 
                   indexFreqAnalysis=length(HRVData$FreqAnalysis),
                   ULFmin=0, ULFmax=0.03, 
                   VLFmin=0.03, VLFmax=0.05,
                   LFmin=0.05, LFmax=0.15, 
                   HFmin=0.15, HFmax=0.4,
                   log="y", type="l",
                   xlab="Frequency (Hz) ", ylab="Spectrum",
                   main=NULL,
                   xlim=c(min(ULFmin, ULFmax, 
                              VLFmin, VLFmax,
                              LFmin, LFmax, 
                              HFmin, HFmax),
                          max(ULFmin, ULFmax, 
                              VLFmin, VLFmax,
                              LFmin, LFmax, 
                              HFmin, HFmax)),
                   ylim=NULL,
                   addLegend=TRUE,
                   addSigLevel=TRUE,
                   usePalette=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                "#F0E442"),
                   ...){
  
  CheckAnalysisIndex(indexFreqAnalysis, length(HRVData$FreqAnalysis),
                        "frequency")
  # check if the periodogram has been computed
  CheckPeriodogram(HRVData, indexFreqAnalysis)
  
  method = HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram$method
  freq = HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram$freq
  spect = HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram$spec
  # if we are plotting a lomb periodogram, check if the plotting range is 
  # larger than the computed frequencies
  if ((method == "Lomb Periodogram") && (max(freq) < max(ULFmin, ULFmax, 
                                                         VLFmin, VLFmax,
                                                         LFmin, LFmax, 
                                                         HFmin, HFmax))){
    warning(paste("Plotting larger frequency range than computed.",
            "Increase the 'to' parameter in CalculatePSD()"))
  }
  # modify main if not supplied
  if(is.null(main)) main = method
  if(is.null(ylim)) ylim = range(spect[is.finite(spect)])
  # avoid using 0 in freq if log="x", log="xy" or log="yx"
  if (log== "x" || log == "xy" || log == "yx"){
    if (freq[[1]] == 0){
      freq = freq[-1]
      spect = spect[-1]
    }
    if (xlim[[1]] == 0){
      xlim[[1]] = freq[[1]]
    }
  }
  plot(freq,spect, xlab=xlab, ylab=ylab, main=main, 
       xlim=xlim, ylim=ylim,
       type=type,
       log=log,...)
  oldPalette = palette()
  on.exit(palette(oldPalette))
  palette(usePalette)
    
  plotFrequencyBands(freq,spect, 
                     ULFmin, ULFmax, 
                     VLFmin, VLFmax,
                     LFmin, LFmax, 
                     HFmin, HFmax, ylim)
  if(addLegend){
    legendVars = buildPeriodogramLegend(ULFmin, ULFmax, VLFmin, VLFmax, 
                                   LFmin, LFmax, HFmin, HFmax)
    legend("topright",
           legendVars$msg,            
           lwd=3,col=legendVars$col,bty="n")
  }
  
  if(addSigLevel &&  (method == "Lomb Periodogram")){
    abline(h=HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram$sig.level,
           lwd=2,col=1,lty=2)
  }
}

buildPeriodogramLegend = function(ULFmin, ULFmax, VLFmin, VLFmax, 
                                  LFmin, LFmax, HFmin, HFmax){
  msg = c()
  col = c()
  if ( (!is.null(ULFmin)) && (!is.null(ULFmax)) ){
    msg = c(msg, paste( "ULF: [",round(ULFmin,digits=2),", ",round(ULFmax,digits=2),"] Hz",sep=""))
    col = c(col,2)
  }
  if ( (!is.null(VLFmin)) && (!is.null(VLFmax)) ){
    msg = c(msg, paste( "VLF: [",round(VLFmin,digits=2),", ",round(VLFmax,digits=2),"] Hz",sep=""))
    col = c(col,3)
  }
  if ( (!is.null(LFmin)) && (!is.null(LFmax)) ){
    msg = c(msg, paste( "LF:   [",round(LFmin,digits=2),", ",round(LFmax,digits=2),"] Hz",sep=""))
    col = c(col,4)
  }
  if ( (!is.null(HFmin)) && (!is.null(HFmax)) ){
    msg = c(msg, paste( "HF:   [",round(HFmin,digits=2),", ",round(HFmax,digits=2),"] Hz",sep=""))
    col = c(col,5)
  }
  list(msg=msg,col=col) 
}
# Private function for distinguishing the different frequency bands of the 
# PSD
plotFrequencyBands = function(freq,spect, 
                              ULFmin, ULFmax, 
                              VLFmin, VLFmax,
                              LFmin, LFmax, 
                              HFmin, HFmax, ylim){
  
  # modify boundaries to avoid gaps in the plot between
  # bands
  if( (!is.null(ULFmax)) && (!is.null(VLFmin)) && (ULFmax == VLFmin) )
    ULFmax = VLFmin = tail(freq[freq <= ULFmax],1)
  if( (!is.null(VLFmax)) && (!is.null(LFmin)) && (VLFmax == LFmin) )
    VLFmax = LFmin = tail(freq[freq <= VLFmax],1)
  if( (!is.null(LFmax)) && (!is.null(HFmin)) && (LFmax == HFmin) )
    LFmax = HFmin = tail(freq[freq <= LFmax],1)
  plotUnderCurve(freq, spect, xlim=c(ULFmin,ULFmax), ylim=ylim, col=2)
  plotUnderCurve(freq, spect, xlim=c(VLFmin,VLFmax), ylim=ylim, col=3)
  plotUnderCurve(freq, spect, xlim=c(LFmin,LFmax), ylim=ylim, col=4)
  plotUnderCurve(freq, spect, xlim=c(HFmin,HFmax), ylim=ylim, col=5)
}

# Private function for highlightning the area under a curve
plotUnderCurve = function(x,y,xlim,ylim,col){
  # if some member of xlim is null => length(xlim) < 2
  if ( length(xlim) == 2) {
    index = which(x >= xlim[[1]] & x <= xlim[[2]])
    if (length(index) == 0) {
      stop("Band out of bounds")
    }
    coordPolX =  x[index]
    coordPolY = y[index]
    # Close the polygon
    coordPolX = c(coordPolX[[1]], coordPolX, tail(coordPolX,1))
    coordPolY = c(min(ylim), coordPolY, min(ylim))
    polygon(coordPolX ,coordPolY, col = col)
  }
}


########################### CalculatePSDBandsEnergy ###########################
#' CalculateSPDBandsEnergy
#' @description Calculates the Energy in the bands of the Power Spectral Density
#' (PSD).
#' @inheritParams PlotPSD
#' @return A vector containing the energy of the ULF, VLF, LF and HF bands in the
#' PSD.
#' @examples
#' \dontrun{
#' data(HRVData)
#' HRVData=BuildNIHR(HRVData)
#' HRVData=FilterNIHR(HRVData)
#' # Frequency analysis requires interpolated data (except Lomb)
#' HRVData=InterpolateNIHR(HRVData)
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,1,"pgram",doPlot = F)
#' # get Energy in the default ULF, VLF and LF frequency bands.
#' # We modify the limits for the HF band
#' CalculateEnergyInPSDBands(HRVData, 1, HFmin = 0.15, HFmax = 0.3) 
#' }
#' @seealso \code{\link{PlotPSD}}, \code{\link{CalculatePSD}}. 
CalculateEnergyInPSDBands = function(HRVData,
                                   indexFreqAnalysis = length(HRVData$FreqAnalysis),
                                   ULFmin=0, ULFmax=0.03, 
                                   VLFmin=0.03, VLFmax=0.05,
                                   LFmin=0.05, LFmax=0.15, 
                                   HFmin=0.15, HFmax=0.4){
  
  CheckAnalysisIndex(indexFreqAnalysis, length(HRVData$FreqAnalysis),
                     "frequency")
  # check if the periodogram has been computed
  CheckPeriodogram(HRVData, indexFreqAnalysis)  
  
  psd = HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram
  c(getEnergyInBand(psd,ULFmin, ULFmax),
    getEnergyInBand(psd,VLFmin, VLFmax),
    getEnergyInBand(psd,LFmin, LFmax),
    getEnergyInBand(psd,HFmin, HFmax))

}


getEnergyInBand = function(psd,freq.min, freq.max){
  
  if (freq.min == freq.max){
    return(0)
  }
  
  if (freq.min > freq.max) {
    stop("The band limits are not properly defined")
  }
  if (freq.min > max(psd$freq)){
    stop("The band's lower frequency is greater than the maximum reachable frequency")
  }
  if (freq.max < min(psd$freq)){
    stop("The band's higher frequency is smaller than the minimun reachable frequency")
  }
  
  indx = getIndexValuesInRange(psd$freq, c(freq.min,freq.max))
  if (length(indx) == 0){
    stop("No values in the specified frequency band")
  }else{
    return(sum(psd$spec[indx]))
  }
  
}

############################## EstimatePSDSlope ##########################
#' Estimate the slope of the Power Spectral Density (PSD). 
#' @description Estimate the slope of the Power Spectral Density (PSD)  of the 
#' RR time series.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param indexFreqAnalysis An integer referencing the periodogram that will be
#' used for estimating the spectral index.
#' @param indexNonLinearAnalysis An integer referencing the structure that will
#'  store the resulting estimations. 
#' @param regressionRange Range of frequencies in which the regression will be
#' performed. Default is c(1e-4, 1e-2) Hz.
#' @param doPlot Plot the periodogram and the least-squares fit?
#' @param main Title for the plot.
#' @param xlab Title for the x axis.
#' @param ylab Title for the y axis.
#' @param pch Symbol for the plotting points.
#' @param log A character string which contains "x" if the x axis is to be 
#' logarithmic, "y" if the y axis is to be logarithmic and "xy" or "yx" if 
#' both axes are to be logarithmic (default).
#' @param ... Other arguments for the plotting function.
#' @details The power spectrum of most physiological signals fulfils
#' \eqn{S(f)=Cf^{-\beta}}{S(f)=C*f^-B} (1/f spectrum). This function estimates
#' the \eqn{\beta}{B} exponent, which is usually referred to as the spectral
#' index. 
#' @note It should be noted that the PSD must be estimated prior to the
#' use of this function. We do not recommend the use of the AR spectrum when
#' estimating the spectral index.
#' @return  The \emph{EstimatePSDSlope} returns the \emph{HRVData} structure 
#' containing a \emph{PSDSlope} field storing the spectral index and the 
#' proper Hurst exponent. 
#' @examples
#' \dontrun{
#' data(HRVProcessedData)
#' # use other name for convenience
#' HRVData=HRVProcessedData
#' # Estimate the periodogram
#' HRVData=CreateFreqAnalysis(HRVData)
#' HRVData=CalculatePSD(HRVData,1,"pgram",doPlot = T,log="xy")
#' HRVData=CreateNonLinearAnalysis(HRVData)
#' HRVData=SetVerbose(HRVData,T)
#' HRVData=EstimatePSDSlope(HRVData,1,1,
#'                         regressionRange=c(5e-4,1e-2))
#' }
#' @references Voss, Andreas, et al. "Methods derived from nonlinear dynamics 
#' for analysing heart rate variability." Philosophical Transactions of the 
#' Royal Society A: Mathematical, Physical and Engineering Sciences 367.1887 
#' (2009): 277-296.
#' 
#' Eke, A., Herman, P., Kocsis, L., & Kozak, L. R. (2002). Fractal 
#' characterization of complexity in temporal physiological signals. 
#' Physiological measurement, 23(1), R1.
#' @seealso \code{\link[stats]{spectrum}},\code{\link[lomb]{lsp}},
#'  \code{\link[RHRV]{CalculatePSD}}.
EstimatePSDSlope = function(HRVData, 
                            indexFreqAnalysis = length(HRVData$FreqAnalysis),
                            indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
                            regressionRange=NULL,doPlot = T,
                            main="PSD power law", xlab = "Frequency (Hz)",
                            ylab="Spectrum", pch = NULL,
                            log="xy",...){
  CheckAnalysisIndex(indexFreqAnalysis,length(HRVData$FreqAnalysis),"frequency")
  CheckAnalysisIndex(indexNonLinearAnalysis,length(HRVData$NonLinearAnalysis),
                                                   "nonlinear")     
  CheckPeriodogram(HRVData, indexFreqAnalysis)
  
  VerboseMessage(HRVData$Verbose,"Calculating spectral index")
  
  freq = HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram$freq
  spec = HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram$spec
  freqRange = range(freq)
  if (is.null(regressionRange)){
    regressionRange = selectRegressionRange(freqRange)
  }
  checkRegressionRange(freqRange,regressionRange)
  
  indx = getIndexValuesInRange(freq, regressionRange)
  lfreq = log(freq)[indx]
  lspec = log(spec)[indx]
  fit = lm(lspec ~ lfreq)
  # plot
  if (doPlot){
    if (is.null(pch)){
      pch = rep(1,length(freq))
      pch[indx] = 3
    }
    VerboseMessage(HRVData$Verbose,"Plotting PSD")
    plot(freq, spec,log=log,main=main,xlab=xlab,ylab=ylab,pch=pch,...)
    lines(exp(lfreq),exp(fit$fitted.values),col=2)
  }
  # get values of beta and H
  beta = -coef(fit)[[2]]
  VerboseMessage(HRVData$Verbose,paste("Spectral index =",rhrvFormat(beta)))
  if (beta > 1){
    HfBm = (beta - 1) / 2
    HfGn = NULL
    VerboseMessage(HRVData$Verbose,paste("H_fBm =",rhrvFormat(HfBm)))
  }else{
    HfBm = NULL 
    HfGn = (beta + 1) / 2
    VerboseMessage(HRVData$Verbose,paste("H_fGn =",rhrvFormat(HfGn)))
  }  
  
  HRVData$NonLinearAnalysis[[indexNonLinearAnalysis]]$PSDSlope = 
    list(computations=list(indexFreqAnalysis=indexFreqAnalysis, 
                           regressionRange=regressionRange),
         statistic=list(beta=beta,HfBm=HfBm,HfGn=HfGn))
  
  HRVData
}

checkRegressionRange = function(freqRange, regressionRange){
  if ( (freqRange[[1]] > regressionRange[[1]]) ||
         (freqRange[[2]] < regressionRange[[2]])){
    stop("Invalid Regression range !!")
  }
}

selectRegressionRange = function(freqRange){
  # typical range is 1e-4 to 1e-2 Hz (Voss et al)
  kTypicalMinFreq = 1e-4 
  kTypicalMaxFreq = 1e-2
  
  if ( (freqRange[[1]] <= kTypicalMinFreq) && (freqRange[[2]] >= kTypicalMaxFreq) ){
    regressionRange = c(1e-4,1e-2)
  }else{
    minFreq = max(kTypicalMinFreq, freqRange[[1]])
    maxFreq = min(kTypicalMaxFreq, freqRange[[2]])
    regressionRange = c(minFreq, maxFreq)
  }
  regressionRange
}

