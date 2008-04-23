\name{plot_spectrogram}
\alias{plot_spectrogram}
\title{ Plots spectrogram }
\description{
  Plots spectrogram of the heart rate signal as calculated by spectrogram() function
}
\usage{
plot_spectrogram(specgr, fsamp = 4, scale = "linear")
}
\arguments{
  \item{specgr}{ spectrogram calculated with spectrogram() }
  \item{fsamp}{ sampling frequency }
  \item{scale}{ scale used to plot spectrogram, linear or logarithmic }
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{spectrogram}} for spectrogram calculation
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
  ## Plotting Spectrogram
  plot_spectrogram(specgr)
}
\keyword{ hplot }
