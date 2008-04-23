\name{load_beat_file}
\alias{load_beat_file}
\title{ Build an array of beats positions from and ascii file }
\description{
  Reads an ascii file with data of beat positions and creates a vector with beat positions in miliseconds
}
\usage{
load_beat_file(file)
}
\arguments{
  \item{file}{ the ascii file to be read }
}
\value{
  A integer array containing the beats positions in miliseconds
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
