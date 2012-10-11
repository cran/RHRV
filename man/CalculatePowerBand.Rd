\name{CalculatePowerBand}
\alias{CalculatePowerBand}
\title{ Calculates power per band }
\description{
  Calculates the spectrogram of the heart rate signal and the power of the spectrogram of the heart rate signal at the HF, LF, VLF bands
}
\usage{
CalculatePowerBand(HRVData, indexFreqAnalysis = -1, size, shift, 
		sizesp = 1024, scale = "linear", ULFmin = 0, ULFmax = 0.03, 
		VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, 
		HFmin = 0.15, HFmax = 0.4, verbose = NULL, type = "fourier", 
		wavelet = "d4", bandtolerance = 0.1, relative = FALSE)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{indexFreqAnalysis}{ Reference to the data structure that will contain the variability analysis }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Points for calculating spectrogram (zero padding) }
  \item{scale}{ Scale used to plot spectrogram, linear or logarithmic }
  \item{ULFmin}{ Lower limit ULF band }
  \item{ULFmax}{ Upper limit ULF band }
  \item{VLFmin}{ Lower limit VLF band }
  \item{VLFmax}{ Upper limit VLF band }
  \item{LFmin}{ Lower limit LF band }
  \item{LFmax}{ Upper limit LF band }
  \item{HFmin}{ Lower limit HF band }
  \item{HFmax}{ Upper limit HF band }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
  \item{type}{ Type of analysis used to calculate the spectrogram. Possible options are "fourier" or "wavelet"} 
  \item{wavelet}{ Mother wavelet used to calculate the spectrogram when a wavelet-based analysis is performed. The available wavelets are: "haar" wavelet; least asymmetric Daubechies wavelets of width 8 ("la8"), 16 ("la16") and 20 ("la20") samples; extremal phase Daubechies of width 4 ("d4"), 6 ("d6"), 8 ("d8") and 16 ("d16") samples; best localized wavelets of width 14 ("bl14") and 20 (" bl20") samples; Fejer-Korovkin wavelets of width 4 ("fk4"), 6 ("fk6"), 8 ("fk8"), 14("fk14") and 22 ("fk22") samples; minimum bandwidth wavelets of width 4 ("mb4"), 8 ("mb8"), 16 ("mb16") and 24 ("mb24"); and the biorthogonal wavelet "bs3.1"}
  \item{bandtolerance}{ Maximum error allowed when a wavelet-based analysis is performed. It can be specified as a absolute or a relative error depending on the "relative" parameter value}
  \item{relative}{ Logic value specifying which kind of bandtolerance shall be used (relative or absolute). The relative tolerance takes into account the width of each of the intervals of interest.}
}
\value{
  Returns HRVData, the structure that contains beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, and the analysis structure including the power of the spectrogram at different bands of the heart rate signal
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculateSpectrogram}} for spectrogram calculation
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
