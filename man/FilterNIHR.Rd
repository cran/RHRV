\name{FilterNIHR}
\alias{FilterNIHR}
\title{ Artefact filter based in an adaptive threshold }
\description{
  An algorithm that uses adaptive thresholds for rejecting those beats different from the given threshold more than a certain value. The rule for beat acceptation or rejection is to compare with previous, following and with the updated mean. We apply also a comparison with acceptable physiological values (25 and 200 bpm). 
}
\usage{
FilterNIHR(HRVData, long = 50, mini = 12, maxi = 20, fixed = 10, last = 13, verbose = FALSE)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{long}{ Number of beats to calculate the updated mean }
  \item{mini}{ Minimum threshold }
  \item{maxi}{ Maximum threshold }
  \item{fixed}{ Fixed part of the threshold }
  \item{last}{ Initial threshold }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns HRVData, the structure that contains beat positions register, associated heart rate instantaneous values also, and now filtered heart rate signal
}
\references{
X. Vila, F. Palacios, J. Presedo, M. Fernandez-Delgado, P. Felix, S. Barro, "Time-Frequency analysis of heart-rate variability," IEEE Eng. Med. Biol. Magazine 16, 119-125 (1997)
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
