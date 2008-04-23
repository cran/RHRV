\name{wfdb_import}
\alias{wfdb_import}
\title{ Import data from a record in wfdb format }
\description{
Initially, the heart beats of the ECG recordings had to be identified and labeled. For this task, an automated function downloaded from the Physionet website and written in C language was used and incorporated to our R software package. Basically, this algorithm reads the annotation file for the ECG register, and writes a text-format translation of it.
}
\usage{
wfdb_import(filein, annotator = "qrs", PATH = ".")
}
\arguments{
  \item{filein}{ the ECG file to be used}
  \item{annotator}{ the type of annotation wished }
  \item{PATH}{ the path of the ECG file }
}
\value{
  No value is returned, the result of applying this function is an output file in the same directory of the input file with the same name of the input adding "\_ascii"
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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
