\name{RHRV-package}
\alias{RHRV-package}
\alias{RHRV}
\docType{package}
\title{
RHRV: An R-based software package for the heart rate variability analysis of ECG recordings
}
\description{
R-HRV, offers functions for performing power spectral analysis of heart rate data. We will use this package for the study of several diseases, such as obstructive sleep apnea or chronic obstructive pulmonary disease.
}
\details{
\tabular{ll}{
Package: \tab RHRV\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-04-23\cr
License: \tab Free\cr
}
This is a package for developping heart rate varibility studies of ECG records. Data are readed from an ascii file containg a column with beat postions in seconds. A function is included in order to build this file from an ECG record in WFDB format (visit the site http://www.physionet.org for more information).
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila

Maintainer: <anton@uvigo.es>
}
\note{
  An example including all the necessary steps to obtain the power bands of a wfdb register is giving below:
  \preformatted{
    ## Reading a wfdb register and converting into ascii format
    wfdb_import("register_name",PATH="register_path")
    ## Loading register
    beat=load_beat_file("register_path\register_name_ascii")
    ## Calculating heart rate signal
    hr=build_hr(beat)
    ## Filtering heart rate signal
    hr2=hr_filter(hr)
    ## Interpolating heart rate signal
    hr3=hr_interpolator(hr2)
    ## Calculating spectrogram
    specgr=spectrogram(hr3,120,5,4)
    ## Calculating power per band
    pw=power_band(specgr)
    ## Plotting power
    plot_power_band(pw)
  }
}  
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\keyword{ package }
