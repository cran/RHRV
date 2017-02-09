\name{RHRV-package}
\alias{RHRV-package}
\alias{RHRV}
\docType{package}
\title{
RHRV: An R-based software package for the heart rate variability analysis of ECG recordings
}
\description{
RHRV offers functions for performing power spectral analysis of heart rate data. We will use this package for the study of several diseases, such as obstructive sleep apnoea or chronic obstructive pulmonary disease.
}
\details{
\tabular{ll}{
Package: \tab RHRV\cr
Type: \tab Package\cr
Version: \tab 4.2.3\cr
Date: \tab 2017-02-09\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
This is a package for developing heart rate variability studies of ECG records. Data are read from an ascii file containing a column with beat positions in seconds. A function is included in order to build this file from an ECG record in WFDB format (visit the site http://www.physionet.org for more information).
}
\author{
A. Mendez, L. Rodriguez, A. Otero, C.A. Garcia, X. Vila, M. Lado

Maintainer: Leandro Rodriguez-Linares <leandro@uvigo.es>
}
\note{
  An example including all the necessary steps to obtain and to \cr 
	  analyze by episodes the power bands of a wfdb register is \cr
	  giving below:\cr

	##Reading a wfdb register and storing into a data structure:\cr
    md = CreateHRVData(Verbose = TRUE)\cr
    md = LoadBeatWFDB(md, RecordName = "register_name", \cr
			RecordPath = "register_path")\cr
	
	##Loading information of episodes of apnea:\cr
	md = LoadApneaWFDB(md, RecordName = "register_name", \cr
			RecordPath = "register_path", Tag = "APN")\cr
	
	##Generating new episodes before and after previous episodes of \cr
	apnea:\cr
	md = GenerateEpisodes(md, NewBegFrom = "Beg", NewEndFrom = "Beg", \cr
			DispBeg = -600, DispEnd = -120, OldTag = "APN", \cr
			NewTag = "PREV_APN")\cr
	md = GenerateEpisodes(md, NewBegFrom = "End", NewEndFrom = "End", \cr
			DispBeg = 120, DispEnd = 600, OldTag = "APN", \cr
			NewTag = "POST_APN")\cr
   	
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
   	
	##Plotting power per band, including episodes information:\cr
    PlotPowerBand(md, indexFreqAnalysis = 1, hr = TRUE, ymax = 2400000, \cr
			ymaxratio = 3, Tag = "all")\cr
	
	##Splitting power per band using episodes before and after \cr
	episodes of apnea:\cr
	PrevAPN = SplitPowerBandByEpisodes(md, indexFreqAnalysis = 1, \cr
			Tag = "PREV_APN")\cr
	PostAPN = SplitPowerBandByEpisodes(md, indexFreqAnalysis = 1, \cr
			Tag = "POST_APN")\cr
	
	##Performing Student's t-test:\cr
	result = t.test(PrevAPN$InEpisodes$ULF, PostAPN$InEpisodes$ULF)\cr
	print(result)\cr
}  
\references{
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
}
\keyword{ package }
