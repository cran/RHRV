CalculatePowerBand<-
function(HRVData, indexFreqAnalysis=-1, size, shift, sizesp=1024, scale="linear", ULFmin=0, ULFmax=0.03, VLFmin=0.03, VLFmax=0.05, LFmin=0.05, LFmax=0.15, HFmin=0.15, HFmax=0.4, verbose=NULL,type="fourier",wavelet="d4",bandtolerance=0.1,relative=FALSE) {
# -------------------------
# Calculates power per band
# -------------------------
#  indexFreqAnalysis: index of an existing frequency analysis to use
#  size, disp: size and displacement of window (sec.)
#  sizesp: seconds for calculating spectrogram (zero padding)
#  ULF band: from 0 to 0.03Hz
# 	VLF band: from 0.03 to 0.05Hz
# 	LF band: from 0.05 to 0.15Hz
# 	HF band: from 0.15 to 0.4Hz
#   type: type of analysis, "fourier" or "wavelet"
#   wavelet: nama of the wavelet for analysis
#   bandtolerance: bandtolerance in % for the wavelet tree decomposition
  

	if (!is.null(verbose)) {
		cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
		SetVerbose(HRVData,verbose)
	}
	
	if (HRVData$Verbose) {
		cat("** Calculating power per band **\n")
	}

	if (indexFreqAnalysis==-1 ) {
      	stop("  --- Frequency analysis not present ---\n    --- Quitting now!! ---\n")
   	}

 if ((length(HRVData$FreqAnalysis) < indexFreqAnalysis) || (indexFreqAnalysis<1) ) {
	  	stop("   --- Frequency analysis no.",indexFreqAnalysis,"not present!! ---\n    --- Quitting now!! ---\n")
 }
 
 if ((type!="fourier") &&( type!="wavelet")){
       stop("   --- Incorrect type of analysis:",type," ---\n    --- Quitting now!! ---\n")
 }
 
if (( type=="wavelet")&& (bandtolerance< 0)){
       stop("   --- Band tolerance:",bandtolerance,"%, must be positive:",type," ---\n    --- Quitting now!! ---\n")
 }
 
 if (( type=="wavelet")&& (max(ULFmin,ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax)>HRVData$Freq_HR)){
       stop("   --- bandtolerance:",max(ULFmin,ULFmax, VLFmin, VLFmax, LFmin, LFmax, HFmin, HFmax)," bigger than sampling frequency. ---\n    --- Quitting now!! ---\n")
 }
 
 
  if (type=="fourier"){
          if (HRVData$Verbose) {
              cat("** Using Fourier analysis **\n")
        	}
        	specgr=CalculateSpectrogram(HRVData,size,shift,sizesp)
        
        	num_frames=dim(specgr)[1]
        	num_points=dim(specgr)[2]
        	fsamp=HRVData$Freq_HR
        
        	if (HRVData$Verbose) {
        		cat("   Number of frames: ",num_frames, "\n")
        		cat("   Number of points: ",num_points, "\n")
        	}
          
          	# absolute power per band
        	for (i in 1:num_frames) {
        		if ((ULFmin*num_points/(fsamp/2)) <= 2) {
        			HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[i]=sum(specgr[i,2:(ULFmax*num_points/(fsamp/2))]) #ULF
        		} else {
        			HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF[i]=sum(specgr[i,(ULFmin*num_points/(fsamp/2)):(ULFmax*num_points/(fsamp/2))]) #ULF
        		}
        		
        		if ((VLFmin*num_points/(fsamp/2)) <= 2) {
        			HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[i]=sum(specgr[i,2:(VLFmax*num_points/(fsamp/2))]) #VLF
        		} else {
        			HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF[i]=sum(specgr[i,(VLFmin*num_points/(fsamp/2)):(VLFmax*num_points/(fsamp/2))]) #VLF
        		}
        		HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i]=sum(specgr[i,(LFmin*num_points/(fsamp/2)):(LFmax*num_points/(fsamp/2))]) #LF
        		HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i]=sum(specgr[i,(HFmin*num_points/(fsamp/2)):(HFmax*num_points/(fsamp/2))]) #HF
            HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF[i]=HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF[i]/HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF[i] #LF/HF
        		HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV[i]=sum(specgr[i,2:dim(specgr)[2]]) #Total
        	}
          #metadata for fourier analysis
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$size=size
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$shift=shift
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$sizesp=sizesp

  }
   if (type=="wavelet"){
          if (HRVData$Verbose) {
              cat("** Using Wavelet analysis **\n")
          }
          
          
          
          powers=modwptAnalysis(HRVData$HR,wavelet, ULFmin, ULFmax , VLFmin, VLFmax, LFmin, LFmax, HFmin , HFmax, HRVData$Freq_HR,bandtolerance,relative)
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$ULF=powers$ULF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$VLF=powers$VLF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF=powers$LF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$HF=powers$HF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$HRV=powers$ULF+powers$VLF+powers$LF+powers$HF
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$LFHF=powers$LF/powers$HF
          #metadata for wavelet analysis
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$wavelet=wavelet
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$bandtolerance=bandtolerance
          HRVData$FreqAnalysis[[indexFreqAnalysis]]$depth=powers$depth
          # rule of thumb used to determine if wavelet analysis is descending too many levels
          L=length(wave.filter(wavelet)$lpf)
          N=length(HRVData$HR)
          J=log2(N/(L-1)+1)                
          if ((HRVData$Verbose)&&(powers$depth>J)){
              cat("** Warning: Wavelet analysis requires descending too many levels **\n")
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
 
  
	if (HRVData$Verbose) {
		cat("Power per band calculated\n")
	}
	return(HRVData)
}
