\name{PlotSpectrogram}
\alias{PlotSpectrogram}
\title{ Calculates and Plots spectrogram }
\description{
  Plots spectrogram of the heart rate signal as calculated by CalculateSpectrogram() function
}
\usage{
PlotSpectrogram(Data, size, shift, sizesp = 1024, scale = "linear", verbose = FALSE)
}

\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{size}{ Size of window for calculating spectrogram (seconds) }
  \item{shift}{ Displacement of window for calculating spectrogram (seconds) }
  \item{sizesp}{ Seconds for calculating spectrogram (zero padding) }
  \item{scale}{ Scale used to plot spectrogram, linear or logarithmic }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
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
    md=BuildNIHR(md,verbose=TRUE)
    md=FilterNIHR(md,verbose=TRUE)
    md=InterpolateNIHR(md,verbose=TRUE)
    ## Calculating and Plotting Power per Band
    PlotSpectrogram(md,size=120,shift=10,sizesp=1024,verbose=TRUE)
  }
}
\keyword{ hplot }
