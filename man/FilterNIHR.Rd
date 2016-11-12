\name{FilterNIHR}
\alias{FilterNIHR}
\title{ Artefact filter based in an adaptive threshold }
\description{
  An algorithm that uses adaptive thresholds for rejecting those beats different from the given threshold more than a certain value. The rule for beat acceptation or rejection is to compare with previous, following and with the updated mean. We apply also a comparison with acceptable physiological values (default values 25 and 200 bpm). 
}
\usage{
FilterNIHR(HRVData, long=50, last=13, minbpm=25, maxbpm=200, mini=NULL, 
		maxi=NULL, fixed=NULL, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{long}{ Number of beats to calculate the updated mean }
  \item{last}{ Initial threshold }  
  \item{minbpm}{ Minimum physiologically acceptable value for HR }  
  \item{maxbpm}{ Maximum physiologically acceptable value for HR }  
  \item{mini}{ Deprecated argument maintained for compatibility }
  \item{maxi}{ Deprecated argument maintained for compatibility }
  \item{fixed}{ Deprecated argument maintained for compatibility }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register, associated heart rate instantaneous values also, and now filtered heart rate signal
}
\references{
X. Vila, F. Palacios, J. Presedo, M. Fernandez-Delgado, P. Felix, S. Barro, "Time-Frequency analysis of heart-rate variability," IEEE Eng. Med. Biol. Magazine 16, 119-125 (1997)
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
