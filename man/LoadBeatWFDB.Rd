\name{LoadBeatWFDB}
\alias{LoadBeatWFDB}
\title{ Import data from a record in wfdb format }
\description{
  Initially, the heart beats of the ECG recordings had to be identified and labelled. For this task, an automated function downloaded from the Physionet website and written in C language was used and incorporated to our R software package. Basically, this algorithm reads the annotation file for the ECG register, and stores the information obtained in a data structure.
}
\usage{
LoadBeatWFDB(Data, RecordName, RecordPath = ".", annotator = "qrs", verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{RecordName}{ The ECG file to be used }
  \item{RecordPath}{ The path of the ECG file }
  \item{annotator}{ The type of annotation wished }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that contains beat positions register
}
\note{
  On Windows and Macosx operating systems is necessary to define a .Renviron file in the user workspace indicating the directory of the wfdbtools commands. Examples for both OS are given below:
  \preformatted{
    ## .Renviron on Windows
    PATH="c:\\\cygwin\\\bin"
    DYLD_LIBRARY_PATH="c:\\\cygwin\\\lib"

    ## .Renviron on Macosx
    PATH="/opt/local/bin"
    DYLD_LIBRARY_PATH="/opt/local/bin"
  }
}  
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }