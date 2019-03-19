CalculatePowerBand <-
  function(HRVData,
           indexFreqAnalysis = length(HRVData$FreqAnalysis),
           size, shift, sizesp = NULL, scale = "linear",
           ULFmin = 0, ULFmax = 0.03,
           VLFmin = 0.03, VLFmax = 0.05,
           LFmin = 0.05, LFmax = 0.15,
           HFmin = 0.15, HFmax = 0.4,
           type = c("fourier", "wavelet"), wavelet = "d4",
           bandtolerance = 0.01, relative = FALSE,
           verbose = NULL) {
    # -------------------------
    # Calculates power per band
    # -------------------------
    #  indexFreqAnalysis: index of an existing frequency analysis to use
    #  size, disp: size and displacement of window (sec.)
    #  ULF band: from 0 to 0.03Hz
    #  VLF band: from 0.03 to 0.05Hz
    #  LF band: from 0.05 to 0.15Hz
    #  HF band: from 0.15 to 0.4Hz
    #  type: type of analysis, "fourier" or "wavelet"
    #  wavelet: nama of the wavelet for analysis
    #  bandtolerance: bandtolerance for the wavelet tree decomposition
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    VerboseMessage(HRVData$Verbose, "Calculating power per band")
    CheckAnalysisIndex(indexFreqAnalysis, length(HRVData$FreqAnalysis), 
                       "frequency")
    type = match.arg(type)
    
    if ((type == "wavelet") && (bandtolerance < 0)){
      stop("Band tolerance: ", bandtolerance," must be positive.")
    }
    
    if (max(ULFmin, ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax) > (HRVData$Freq_HR / 2)) {
      stop("Some frequency in the band's limits is bigger than the Nyquist frequency (",
           HRVData$Freq_HR / 2," Hz).")
    }
    
    
    if (type == "fourier") {
      VerboseMessage(HRVData$Verbose, "Using Fourier analysis")
      signal=1000.0/(HRVData$HR/60.0)  # msec.
      signalLen = length(signal)
      shiftsamples = shift*HRVData$Freq_HR
      sizesamples = floor(size*HRVData$Freq_HR)
      
      if (is.null(sizesp)){
        sizesp = 2^ceiling(log2(sizesamples))
      }
      if (sizesp <= sizesamples){
        lenZeroPadding = 0  
      }else{
        lenZeroPadding = sizesp - sizesamples
      }
      
      hamming=0.54-0.46*cos(2*pi*(0:(sizesamples-1))/(sizesamples-1))
      hammingfactor=1.586
      
      power <- function (spec_arg,freq_arg,fmin,fmax) {
        band = spec_arg[freq_arg>=fmin & freq_arg<fmax]
        powerinband = hammingfactor*sum(band)/(2*length(spec_arg)**2)
        
        return(powerinband)
      }
      
      # Calculates the number of windows
      nw=1
      begnw=1
      repeat {
        begnw=begnw+shiftsamples
        if ((begnw+sizesamples-1)>=signalLen) {
          break
        }
        nw=nw+1
      }
      VerboseMessage(HRVData$Verbose, paste("Windowing signal...",nw,"windows"))
      
      
      #frequency axis
      freqs = seq(from=0,to=HRVData$Freq_HR/2,length.out = (sizesamples+lenZeroPadding)/2)
      
      for (i in 1:nw) {
        beg=1+(shiftsamples*(i-1))
        window = signal[beg:(beg + sizesamples - 1)]
        
        window[is.na(window)] = 0
        
        window = window - mean(window)
        window = window * hamming
        window = c(window,rep(0,len = lenZeroPadding))

        spec_tmp=Mod(fft(window))**2
        spec = spec_tmp[1:(length(spec_tmp)/2)]
        
        HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV[i]=power(spec,freqs,0.0,0.5*HRVData$Freq_HR)
        HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[i]=power(spec,freqs,ULFmin,ULFmax)
        HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[i]=power(spec,freqs,VLFmin,VLFmax)
        HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i]=power(spec,freqs,LFmin,LFmax)
        HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i]=power(spec,freqs,HFmin,HFmax)
        
        
        HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF[i]=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i]/HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i]
      }   
      # We set time in the center of each window
      realShiftTime =   shiftsamples / HRVData$Freq_HR
      realSizeTime = sizesamples / HRVData$Freq_HR
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$Time =
        seq(realSizeTime / 2, by = realShiftTime,
            length.out = length(HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF))
      
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$size=size
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$shift=shift
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$sizesp=sizesp
      
    }
    if (type=="wavelet"){
      VerboseMessage(HRVData$Verbose, "Using Wavelet analysis")
      
      signal=1000.0/(HRVData$HR/60.0)  # msec.
      powers=modwptAnalysis(signal,wavelet, ULFmin, ULFmax , VLFmin, VLFmax, LFmin, LFmax, HFmin , HFmax, HRVData$Freq_HR,bandtolerance,relative)
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF=powers$ULF
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF=powers$VLF
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF=powers$LF
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF=powers$HF
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV=powers$ULF+powers$VLF+powers$LF+powers$HF
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF=powers$LF/powers$HF
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$Time=
        seq(head(HRVData$Beat$Time,1), by = 1/HRVData$Freq_HR,
            length.out = length(powers$ULF))
      #metadata for wavelet analysis
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$wavelet=wavelet
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$bandtolerance=bandtolerance
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$depth=powers$depth
      # rule of thumb used to determine if wavelet analysis is descending too many levels
      L=length(wave.filter(wavelet)$lpf)
      N=length(HRVData$HR)
      J = log2(N / (L - 1) + 1)                
      if (powers$depth > J) {
        warning("Wavelet analysis requires descending too many levels")
      }
      
    }
    #common metadata for both analysis
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$type=type
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULFmin=ULFmin
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULFmax=ULFmax
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLFmin=VLFmin
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLFmax=VLFmax
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFmin=LFmin
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFmax=LFmax
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$HFmin=HFmin
    HRVData$FreqAnalysis[[indexFreqAnalysis]]$HFmax=HFmax
    
    VerboseMessage(HRVData$Verbose, "Power per band calculated")
    
    return(HRVData)
  }


############################## getNormSpectralUnits ##########################
#' Normalized Spectral Units
#' @description
#' Calculates the spectrogram bands in normalized units
#' @details 
#' The default behaviour of this function computes the normalized power
#' time series in the LF and HF bands following the Task Force recommendations:
#' 
#' \deqn{normalized\_LF = LF\_power / (total\_power - VLF\_power - ULF\_power)}{
#' normalized_LF = LF_power / (total_power - VLF_power - ULF_power)}
#' \deqn{normalized\_HF = HF\_power / (total\_power - VLF\_power -ULF\_power)}{
#' normalized_HF = HF_power / (total_power - VLF_power -ULF-power)}
#' 
#' If \emph{VLFnormalization} is set to FALSE, the functions computes:
#' \deqn{normalized\_VLF = VLF\_power / (total\_power - ULF\_power)}{
#' normalized_VLF = VLF_power / (total_power - ULF_power)}
#' \deqn{normalized\_LF = LF\_power / (total\_power - ULF\_power)}{
#' normalized_LF = LF_power / (total_power - ULF_power)}
#' \deqn{normalized\_HF = HF\_power / (total\_power - ULF\_power)}{
#' normalized_HF = HF_power / (total_power - ULF_power)}
#' 
#' The resulting time series are returned in a list. Note that before using this
#' function, the spectrogram should be computed with the \emph{CalculatePowerBand}
#' function.
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param indexFreqAnalysis Reference to the data structure that contains the spectrogram analysis
#' @param VLFnormalization Logical value. If TRUE (default), the function
#' normalizes LF and HF power series by its sum. If FALSE, the function computes
#' VLF, LF and HF power series  by its sum.
#' @return  The \emph{getNormSpectralUnits} returns a list storing the resulting
#' normalized power-band series. Note that this list is not stored in the 
#' \emph{HRVData} structure.
#' @references Camm, A. J., et al. "Heart rate variability: standards of measurement, physiological interpretation and clinical use.
#' Task Force of the European Society of Cardiology and the North American Society of
#' Pacing and Electrophysiology." Circulation 93.5 (1996): 1043-1065.
#' @examples
#' \dontrun{
#' # load some data...
#' data(HRVProcessedData)
#' hd = HRVProcessedData
#' # Perform some spectral analysis and normalize the results
#' hd = CreateFreqAnalysis(hd)
#' hd = CalculatePowerBand(hd,indexFreqAnalysis = 1,shift=30,size=60)
#' normUnits = getNormSpectralUnits(hd)
#' # plot the normalized time series
#' par(mfrow=c(2,1))
#' plot(normUnits$Time, normUnits$LF, xlab="Time", ylab="normalized LF",
#'      main="normalized LF",type="l")
#' plot(normUnits$Time, normUnits$HF, xlab="Time", ylab="normalized HF",
#'      main="normalized HF",type="l")
#' par(mfrow=c(1,1))
#' 
#' }
#' @rdname getNormSpectralUnits
getNormSpectralUnits <- function(HRVData,
                                 indexFreqAnalysis = length(HRVData$FreqAnalysis),
                                 VLFnormalization = T){
  CheckAnalysisIndex(indexFreqAnalysis, length(HRVData$FreqAnalysis),
                     "frequency")
  # check if the spectromgram has been computed
  CheckPowerBand(HRVData, indexFreqAnalysis)
  
  Time = HRVData$FreqAnalysis[[indexFreqAnalysis]]$Time
  # if VLFnormalization = T
  # normalization factor is (totalPower - ULF-power - VLFpower) 
  # else normalization factor is (totalPower - ULF-power) 
  if (VLFnormalization){
    normFactor <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF +
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF  
  }else{
    normFactor <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF +
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF +
      HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF
  }
  
  LFnorm <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF / normFactor
  HFnorm <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF / normFactor
  
  if (!VLFnormalization){
    # if we don't normalize by the VLF norm we may return the
    # VLFpower normalized
    VLFnorm <- HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF / normFactor
    return(list(VLF=VLFnorm,LF=LFnorm,HF=HFnorm,Time=Time))
  }else{
    return(list(LF=LFnorm,HF=HFnorm,Time=Time))
  }
  
}
