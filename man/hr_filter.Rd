\name{hr_filter}
\alias{hr_filter}
\title{ Artefact filter based in an adaptive threshold }
\description{
  An algorithm that uses adaptive thresholds for rejecting those beats different from the given threshold more than a certain value. The rule for beat acceptation or rejection is to compare with previous, following
and with the updated mean. We apply also a comparison with acceptable physiological values (25 and 200 bpm). 
}
\usage{
hr_filter(hr, long = 50, mini = 12, maxi = 20, fixed = 10, last = 13)
}
\arguments{
  \item{hr}{ heart rate signal matrix }
  \item{long}{ number of beats to calculate the updated mean }
  \item{mini}{ minimum threshold }
  \item{maxi}{ maximum threshold }
  \item{fixed}{ fixed part of the threshold }
  \item{last}{ initial threshold }
}
\value{
  Filtered heart rate signal matrix
}
\references{
X. Vila, F. Palacios, J. Presedo, M. Fernandez-Delgado, P. Felix, S. Barro, "Time-Frequency analisys of heart-rate variability," IEEE Eng. Med. Biol. Magazine 16, 119-125 (1997).
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
