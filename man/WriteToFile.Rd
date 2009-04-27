\name{WriteToFile}
\alias{WriteToFile}
\title{ Writes data structure to a file }
\description{
  Writes the data structure containing beat positions and all derived calculations to a file
}
\usage{
WriteToFile(Data, name, overwrite = TRUE, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{name}{ The name of the file to be used }
  \item{overwrite}{ Boolean argument for indicating what to do if the file already exists }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }