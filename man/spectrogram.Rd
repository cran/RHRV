\name{spectrogram}
\alias{spectrogram}
\title{ Calculates the spectrogram of a signal }
\description{
  Calculates the spectrogram of the heart rate signal after filtering and interpolation in a window of a certain size 
}
\usage{
spectrogram(signal, size, shift, fsamp = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{ the heart rate signal after filtering and interpolation }
  \item{size}{ size of window (seconds) }
  \item{shift}{ displacement of window (seconds) }
  \item{fsamp}{ sampling frequency }
}
\value{
  A matrix with the module of the FFT of the signal
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\examples{
  ## Signal Creation
  l=1000;
  hamming=0.54-0.46*cos(2*pi*(0:(l-1))/(l-1))
  x=seq(length=l,from=0,by=.25)
  s1=sin(x*2.0*pi*0.025)*hamming
  s2=sin(x*2.0*pi*0.1)*hamming
  s3=sin(x*2.0*pi*0.3)*hamming
  s4=s1+s2+s3
  s=c(s1,s2,s3,s4)
  s=array(c(1:(4*l),s),dim=c(4*l,2))
  ## Calculating Spectrogram
  specgr=spectrogram(s,120,5,4)
}
\keyword{ misc }
