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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An 
R-based software package for heart rate variability analysis of ECG recordings,"
3rd Iberian Conference in Systems and Information Technologies (CISTI 2008),
Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
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

