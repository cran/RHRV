\name{ReadFromFile}
\alias{ReadFromFile}
\title{ Reads data structure from file }
\description{
  Reads the data structure containing beat positions and all derived calculations from file
}
\usage{
ReadFromFile(name, verbose=FALSE)
}
\arguments{
  \item{name}{ The name of the file to be used (without the .hrv extension)}
  \item{verbose}{Logical value that sets the verbose mode on or off}
}
\value{
  Returns the HRVData structure previously stored in the given file.
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }

