\name{CalculateSpectrogram}
\alias{CalculateSpectrogram}
\title{ Calculates the spectrogram of a signal }
\description{
  Calculates the spectrogram of the heart rate signal after filtering and interpolation in a window of a certain size 
}
\usage{
CalculateSpectrogram(HRVData, size, shift, sizesp = 1024, verbose = FALSE)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Seconds for calculating spectrogram (zero padding) }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns the spectrogram of the heart rate signal
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}

\note{
  An example including all the necessary steps to obtain the spectrogram of a wfdb register is giving below:\cr\cr
	\#\#Reading a wfdb register and storing into a data structure:\cr
    md = CreateHRVData(verbose = TRUE)\cr
    md = LoadBeatWFDB(md, RecordName = "register\_name", RecordPath = "register\_path", verbose = TRUE)\cr
   	\#\#Calculating heart rate signal:\cr
    md = BuildNIHR(md, verbose = TRUE)\cr
   	\#\#Filtering heart rate signal:\cr
    md = FilterNIHR(md, verbose = TRUE)\cr
   	\#\#Interpolating heart rate signal:\cr
    md = InterpolateNIHR(md, verbose = TRUE)\cr
   	\#\#Calculating spectrogram:\cr
	CalculateSpectrogram(md, size = 120, shift = 10, sizesp = 1024, verbose = TRUE)\cr
}
\keyword{ misc }
