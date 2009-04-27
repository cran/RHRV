\name{CalculateSpectrogram}
\alias{CalculateSpectrogram}
\title{ Calculates the spectrogram of a signal }
\description{
  Calculates the spectrogram of the heart rate signal after filtering and interpolation in a window of a certain size 
}
\usage{
CalculateSpectrogram(Data, size, shift, sizesp = 1024, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Seconds for calculating spectrogram (zero padding) }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that contains beat positions register, associated heart rate instantaneous values also, filtered heart rate signal equally spaced, and spectrogram of the heart rate signal
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
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
    ## Calculating Spectrogram
    CalculateSpectrogram(md,size=120,shift=10,sizesp=1024,verbose=TRUE)
  }
}
\keyword{ misc }
