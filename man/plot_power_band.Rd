\name{plot_power_band}
\alias{plot_power_band}
\title{ Plots power calculated by power\_band function }
\description{
  Plots the power of the heart rate signal at different bands of interest.
}
\usage{
plot_power_band(power, lsecs, normalized = FALSE, hr = NA, ymax = 160000, ymaxratio = 10, ymaxnorm = 1)
}
\arguments{
  \item{power}{ calculated by power\_band() }
  \item{lsecs}{ seconds for x axis }
  \item{normalized}{ plots normalized powers if TRUE }
  \item{hr}{ heart rate signal }
  \item{ymax}{ maximum value for y axis (unnormalized plots) }
  \item{ymaxratio}{ maximum value for y axis in LF/HF band (normalized and unnormalized plots) }
  \item{ymaxnorm}{ maximum value for y axis (normalized plots) }
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{power_band}} for power calculation
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
  ## Plotting Power per Band
  plot_power_band(pw)
}
\keyword{ hplot }
