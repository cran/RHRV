\name{PlotSpectrogram}
\alias{PlotSpectrogram}
\title{ Calculates and Plots spectrogram }
\description{
  Plots spectrogram of the heart rate signal as calculated by CalculateSpectrogram() function
}
\usage{
PlotSpectrogram(HRVData, size, shift, sizesp=NULL, freqRange = NULL, 
                scale="linear", verbose=NULL) 
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Points for calculation (zero padding). If the user does not specify it, the function estimates a propper value.}
  \item{freqRange}{Vector with two components specifying the frequency range that the program
  should plot. If the user does not specify it, the function uses the whole frequency range.}
  \item{scale}{ Scale used to plot spectrogram, linear or logarithmic }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
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
  An example including all the necessary steps to obtain the power \cr
	  bands of a wfdb register is giving below:\cr

	##Reading a wfdb register and storing into a data structure:\cr
    md = CreateHRVData(Verbose = TRUE)\cr
    md = LoadBeatWFDB(md, RecordName = "register_name", \cr
			RecordPath = "register_path")\cr
   	
	##Calculating heart rate signal:\cr
    md = BuildNIHR(md)\cr
   	
	##Filtering heart rate signal:\cr
    md = FilterNIHR(md)\cr
   	
	##Interpolating heart rate signal:\cr
    md = InterpolateNIHR(md)\cr
   	
	##Calculating and Plotting Spectrogram\cr
    PlotSpectrogram(md, size = 120, shift = 10, sizesp = 1024)\cr
}
\keyword{ hplot }

