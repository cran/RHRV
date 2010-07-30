\name{LoadBeatWFDB}
\alias{LoadBeatWFDB}
\title{ Imports data from a record in wfdb format }
\description{
  Initially, the heart beats of the ECG recordings had to be identified and labelled. For this task, an automated function downloaded from the Physionet website and written in C language was used and incorporated to our R software package. Basically, this algorithm reads the annotation file for the ECG register, and stores the information obtained in a data structure.
}
\usage{
LoadBeatWFDB(HRVData, RecordName, RecordPath = ".", annotator = "qrs", verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The ECG file to be used }
  \item{RecordPath}{ The path of the ECG file }
  \item{annotator}{ The type of annotation wished }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns Data, the structure that contains beat positions register
}
\note{
  On Windows and Macosx operating systems is necessary to define a .Renviron file in the user workspace indicating the directory of the wfdbtools commands. Examples for both OS are given below:
	\cr\cr
	## .Renviron on Windows\cr
    PATH = "c:\\\\cygwin\\\\bin"\cr
    DYLD_LIBRARY_PATH = "c:\\\\cygwin\\\\lib"\cr
	\cr
    ## .Renviron on Macosx\cr
    PATH = "/opt/local/bin"\cr
    DYLD_LIBRARY_PATH = "/opt/local/bin"\cr
}  
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
