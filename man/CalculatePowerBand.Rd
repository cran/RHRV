\name{CalculatePowerBand}
\alias{CalculatePowerBand}
\title{ Calculates power per band }
\description{
  Calculates  power of the heart rate signal at ULF, VLF, LF and HF bands
}
\usage{
CalculatePowerBand(HRVData,
                   indexFreqAnalysis = length(HRVData$FreqAnalysis),
                   size, shift, sizesp = NULL, scale = "linear",
                   ULFmin = 0, ULFmax = 0.03,
                   VLFmin = 0.03, VLFmax = 0.05,
                   LFmin = 0.05, LFmax = 0.15,
                   HFmin = 0.15, HFmax = 0.4,
                   type = c("fourier", "wavelet"), wavelet = "d4",
                   bandtolerance = 0.01, relative = FALSE,
                   verbose = NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information 
  related to it }
  \item{indexFreqAnalysis}{ Reference to the data structure that will contain
  the variability analysis }
  \item{size}{ Size of window for calculations (seconds) }
  \item{shift}{ Displacement of window for calculations (seconds) }
  \item{sizesp}{ Points for calculation (zero padding). If the user does not 
  specify it, the function estimates a propper value.}
  \item{ULFmin}{ Lower limit ULF band }
  \item{ULFmax}{ Upper limit ULF band }
  \item{VLFmin}{ Lower limit VLF band }
  \item{VLFmax}{ Upper limit VLF band }
  \item{LFmin}{ Lower limit LF band }
  \item{LFmax}{ Upper limit LF band }
  \item{HFmin}{ Lower limit HF band }
  \item{HFmax}{ Upper limit HF band }
  \item{scale}{ Deprecated argument }
  \item{verbose}{ Deprecated argument maintained for compatibility,
  use SetVerbose() instead }
  \item{type}{ Type of analysis used to calculate the spectrogram. 
  Possible options are "fourier" or "wavelet"} 
  \item{wavelet}{ Mother wavelet used to calculate the spectrogram when
  a wavelet-based analysis is performed. The available wavelets are: 
  "haar" wavelet; least asymmetric Daubechies wavelets of width 8 ("la8"),
  16 ("la16") and 20 ("la20") samples; extremal phase Daubechies of width 4
  ("d4"), 6 ("d6"), 8 ("d8") and 16 ("d16") samples; best localized wavelets
  of width 14 ("bl14") and 20 (" bl20") samples; Fejer-Korovkin wavelets of 
  width 4 ("fk4"), 6 ("fk6"), 8 ("fk8"), 14("fk14") and 22 ("fk22") samples; 
  minimum bandwidth wavelets of width 4 ("mb4"), 8 ("mb8"), 16 ("mb16") and 24 
  ("mb24"); and the biorthogonal wavelet "bs3.1"}
  \item{bandtolerance}{ Maximum error allowed when a wavelet-based analysis is 
  performed. It can be specified as a absolute or a relative error depending on
  the "relative" parameter value}
  \item{relative}{ Logic value specifying which kind of bandtolerance shall be
  used (relative or absolute). The relative tolerance takes into account the 
  width of each of the intervals of interest.}
}
\value{
  Returns HRVData, the structure that contains beat positions register, 
  associated heart rate instantaneous values, filtered heart rate signal
  equally spaced, and the analysis structure including spectral power at 
  different bands of the heart rate signal
}
\references{
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, 
and I. Gomez-Conde, "An open source tool for heart rate variability spectral 
analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\note{
  An example including all the necessary steps to obtain the power\cr 
	  bands of a wfdb register is giving below:\cr
	
	##Reading a wfdb register and storing into a data structure:\cr
    md = CreateHRVData(Verbose = TRUE)\cr
    md = LoadBeatWFDB(md, RecordName = "register_name",\cr 
			RecordPath = "register_path")\cr
   	
	##Calculating heart rate signal:\cr
    md = BuildNIHR(md)\cr
   	
	##Filtering heart rate signal:\cr
    md = FilterNIHR(md)\cr
   	
	##Interpolating heart rate signal:\cr
    md = InterpolateNIHR(md)\cr
   	
	##Calculating spectrogram and power per band using fourier \cr
	analysis:\cr
    md = CreateFreqAnalysis(md)\cr
    md = CalculatePowerBand(md, indexFreqAnalysis = 1, size = 120, \cr
			shift = 10, sizesp = 1024)\cr
   	
	##Calculating spectrogram and power per band using wavelet analysis:\cr
    md = CreateFreqAnalysis(md)\cr
    md = CalculatePowerBand(md, indexFreqAnalysis = 2, type="wavelet",\cr 
			wavelet="la8",bandtolerance=0.0025)\cr
}
\keyword{ misc }
