\name{LoadApneaWFDB}
\alias{LoadApneaWFDB}
\title{ Loads apnea episodes for WFDB record }
\description{
  Loads the information of apnea episodes and stores it into the data structure containing the beat positions and other related information
}
\usage{
LoadApneaWFDB(HRVData, RecordName, RecordPath = ".", Tag = "APNEA", 
		verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The WFDB file to be used }
  \item{RecordPath}{ The path of the WFDB file }
  \item{Tag}{ to include APNEA episodes }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register and other related information and apnea episodes information
}
\note{
  An example including all the steps to download a record from Physionet and load its content and the Apnea annotations is included below:\cr\cr
  dirorig <-  "http://www.physionet.org/physiobank/database/apnea-ecg/"\cr
  files <- c("a01.hea", "a01.apn", "a01.qrs")\cr
  filesorig <- paste(dirorig, files, sep = "")\cr
  for (i in 1:length(files))\cr
    download.file(filesorig[i], files[i])\cr
  hrv.data <- CreateHRVData()\cr
  hrv.data <- LoadBeatWFDB(hrv.data, "a01")\cr
  hrv.data <- LoadApneaWFDB(hrv.data, "a01")\cr
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
