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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }

