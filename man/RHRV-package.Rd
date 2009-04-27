\name{RHRV-package}
\alias{RHRV-package}
\alias{RHRV}
\docType{package}
\title{
RHRV: An R-based software package for the heart rate variability analysis of ECG recordings
}
\description{
RHRV, offers functions for performing power spectral analysis of heart rate data. We will use this package for the study of several diseases, such as obstructive sleep apnoea or chronic obstructive pulmonary disease.
}
\details{
\tabular{ll}{
Package: \tab RHRV\cr
Type: \tab Package\cr
Version: \tab 1.6.2\cr
Date: \tab 2009-04-27\cr
License: \tab GPL-2\cr
}
This is a package for developing heart rate variability studies of ECG records. Data are read from an ascii file containing a column with beat positions in seconds. A function is included in order to build this file from an ECG record in WFDB format (visit the site http://www.physionet.org for more information).
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila

Maintainer: A. Mendez <mrarthur@uvigo.es>
}
\note{
  An example including all the necessary steps to obtain the power bands of a wfdb register is giving below:
  \preformatted{
    ## Reading a wfdb register and storing into a data structure
    md=CreateData(verbose=TRUE)
    md=LoadBeatWFDB(md,"register_name",PATH="register_path",verbose=TRUE)
    ## Calculating heart rate signal
    md=BuildNIHR(md,verbose=TRUE)
    ## Filtering heart rate signal
    md=FilterNIHR(md,verbose=TRUE)
    ## Interpolating heart rate signal
    md=InterpolateNIHR(md,verbose=TRUE)
    ## Calculating spectrogram and power per band
    md=CalculatePowerBand(md,size=120,shift=10,sizesp=1024,verbose=TRUE)
    ## Plotting power per band
    PlotPowerBand(md,hr=TRUE,ymax=700000,ymaxratio=4,Episodes=TRUE,verbose=TRUE)
  }
}  
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\keyword{ package }
