\name{PlotPowerBand}
\alias{PlotPowerBand}
\title{ Plots power determined by CalculatePowerBand function }
\description{
  Plots the power of the heart rate signal at different bands of interest.
}
\usage{
PlotPowerBand(HRVData, indexFreqAnalysis, normalized = FALSE, 
		hr = FALSE, ymax = 160000, ymaxratio = 10, ymaxnorm = 1, 
		Tag = NULL, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{indexFreqAnalysis}{ Reference to the data structure that will contain the variability analysis }
  \item{normalized}{ Plots normalized powers if TRUE }
  \item{hr}{ Plots heart rate signal if TRUE }
  \item{ymax}{ Maximum value for y axis (unnormalized plots) }
  \item{ymaxratio}{ Maximum value for y axis in LF/HF band (normalized and unnormalized plots) }
  \item{ymaxnorm}{ Maximum value for y axis (normalized plots) }
  \item{Tag}{ Argument that allows to specify if episodes contained in Data are represented by means of coloured boxes, for example apnoea or oxygen desaturation, "ALL" for all episodes}
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\references{
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculatePowerBand}} for power calculation
}
\note{
  An example including all the necessary steps to obtain the power \cr
	  bands of a wfdb register is given below:\cr

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
   	
	##Calculating spectrogram and power per band:\cr
    md = CreateFreqAnalysis(md)\cr
    md = CalculatePowerBand(md, indexFreqAnalysis = 1, size = 120, \cr
			shift = 10, sizesp = 1024)\cr
    
	## Plotting Power per Band\cr
    PlotPowerBand(md, hr = TRUE, ymax = 700000, ymaxratio = 4)\cr
}
\keyword{ hplot }

