\name{CalculatePowerBand}
\alias{CalculatePowerBand}
\title{ Calculates power per band }
\description{
  Calculates the spectrogram of the heart rate signal and the power of the spectrogram of the heart rate signal at the HF, LF, VLF bands
}
 \usage{
CalculatePowerBand(Data, size, shift, sizesp = 1024, scale = "linear", ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Seconds for calculating spectrogram (zero padding) }
  \item{scale}{ Scale used to plot spectrogram, linear or logarithmic }
  \item{ULFmin}{ lower limit ULF band }
  \item{ULFmax}{ upper limit ULF band }
  \item{VLFmin}{ lower limit VLF band }
  \item{VLFmax}{ upper limit VLF band }
  \item{LFmin}{ lower limit LF band }
  \item{LFmax}{ upper limit LF band }
  \item{HFmin}{ lower limit HF band }
  \item{HFmax}{ upper limit HF band }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that contains beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, spectrogram and power of the spectrogram of the heart rate signal
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculateSpectrogram}} for spectrogram calculation
}
\note{
  An example including all the necessary steps to obtain the power bands of a wfdb register is giving below:
  \preformatted{
    ## Heart Rate Signal Creation
    md=CreateData(verbose=TRUE)
    md=LoadBeatAscii(md,"register_name",verbose=TRUE)
    md=LoadEpisodesAscii(md,"episodes_file_name",verbose=TRUE)
    ## Heart Rate Signal Creation
    md=CreateData(verbose=TRUE)
    md=LoadBeatAscii(md,"beat_ascii.txt",verbose=TRUE)
    md=LoadEpisodesAscii(md,"apnea_ascii_tmp.txt",verbose=TRUE)
    md=BuildNIHR(md,verbose=TRUE)
    md=FilterNIHR(md,verbose=TRUE)
    md=InterpolateNIHR(md,verbose=TRUE)
    ## Calculating Power per Band
    md=CalculatePowerBand(md,size=120,shift=10,sizesp=1024,verbose=TRUE)
  }
}
\keyword{ misc }
