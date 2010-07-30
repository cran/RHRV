\name{CalculatePowerBand}
\alias{CalculatePowerBand}
\title{ Calculates power per band }
\description{
  Calculates the spectrogram of the heart rate signal and the power of the spectrogram of the heart rate signal at the HF, LF, VLF bands
}
\usage{
CalculatePowerBand(HRVData, indexFreqAnalysis = -1, size, shift, sizesp = 1024, scale = "linear", ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{indexFreqAnalysis}{ Reference to the data structure that will contain the variability analysis }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Seconds for calculating spectrogram (zero padding) }
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
  An example including all the necessary steps to obtain the power bands of a wfdb register is giving below:\cr\cr
	##Reading a wfdb register and storing into a data structure:\cr
    md = CreateHRVData(Verbose = TRUE)\cr
    md = LoadBeatWFDB(md, RecordName = "register_name", RecordPath = "register_path")\cr
   	##Calculating heart rate signal:\cr
    md = BuildNIHR(md)\cr
   	##Filtering heart rate signal:\cr
    md = FilterNIHR(md)\cr
   	##Interpolating heart rate signal:\cr
    md = InterpolateNIHR(md)\cr
   	##Calculating spectrogram and power per band:\cr
    md = CreateFreqAnalysis(md)\cr
    md = CalculatePowerBand(md, indexFreqAnalysis = 1, size = 120, shift = 10, sizesp = 1024)\cr
}
\keyword{ misc }
