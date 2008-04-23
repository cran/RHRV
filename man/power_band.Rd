\name{power_band}
\alias{power_band}
\title{ Calculates power per band }
\description{
  Calculates the power of the spectrogram of the heart rate signal at the HF, LF, VLF bands
}
\usage{
power_band(specgr, fsamp = 4)
}
\arguments{
  \item{specgr}{ spectrogram of the heart rate signal }
  \item{fsamp}{ sampling frequency }
}
\value{
  A n*m matrix, where n is the dimension of the spectrogram and m is 4, namely HF band power, LF band power, VLF band power and sum of all bands power
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
  ## Calculating Power per Band
  pw=power_band(specgr)
}
\keyword{ misc }
