\name{CalculateSpectrogram}
\alias{CalculateSpectrogram}
\title{ Calculates the spectrogram of a signal }
\description{
  Calculates the spectrogram of the heart rate signal after filtering and 
  interpolation in a window of a certain size 
}
\usage{
CalculateSpectrogram(HRVData, size, shift, sizesp = 1024, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information
  related to it }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Points for calculating spectrogram (zero padding) }
  \item{verbose}{ Deprecated argument maintained for compatibility, use
  SetVerbose() instead }
}
\value{
  Returns the spectrogram of the heart rate signal
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}

\note{
  An example including all the necessary steps to obtain the spectrogram\cr
	  of a wfdb register is giving below:\cr

	##Reading a wfdb register and storing into a data structure:\cr
    md = CreateHRVData(Verbose = TRUE)\cr
    md = LoadBeatWFDB(md, RecordName = "register_name", \cr
			RecordPath = "register_path", verbose = TRUE)\cr

	##Calculating heart rate signal:\cr
    md = BuildNIHR(md)\cr
   	
	##Filtering heart rate signal:\cr
    md = FilterNIHR(md)\cr
   	
	##Interpolating heart rate signal:\cr
    md = InterpolateNIHR(md)\cr
   	
	##Calculating spectrogram:\cr
	CalculateSpectrogram(md, size = 120, shift = 10, sizesp = 1024)\cr
}
\keyword{ misc }

