\name{PlotPowerBand}
\alias{PlotPowerBand}
\title{ Plots power determined by CalculatePowerBand function }
\description{
  Plots the power of the heart rate signal at different bands of interest.
}
\usage{
PlotPowerBand(Data, normalized = FALSE, hr = FALSE, ymax = 160000, ymaxratio = 10, ymaxnorm = 1, Episodes = FALSE, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{normalized}{ Plots normalized powers if TRUE }
  \item{hr}{ Plots heart rate signal if TRUE }
  \item{ymax}{ Maximum value for y axis (unnormalized plots) }
  \item{ymaxratio}{ Maximum value for y axis in LF/HF band (normalized and unnormalized plots) }
  \item{ymaxnorm}{ Maximum value for y axis (normalized plots) }
  \item{Episodes}{ Boolean argument that allows to specify if episodes, apnoea or oxygen desaturation, contained in Data are represented by means of red boxes }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculatePowerBand}} for power calculation
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
    ## Calculating Power per Band
    md=CalculatePowerBand(md,size=120,shift=10,sizesp=1024,verbose=TRUE)
    ## Plotting Power per Band
    PlotPowerBand(md,hr=TRUE,ymax=700000,ymaxratio=4,Episodes=TRUE,verbose=TRUE)
  }
}
\keyword{ hplot }
