#' PlotHR
#' @title Simple plot of interpolated heart rate 
#' @description  Plots in a simple way the interpolated instantaneous heart
#'  rate signal.
#' @param HRVData Data structure that stores the beats register and information 
#' related to it.
#' @param Tags List of tags to specify which episodes, as apnoea or oxygen 
#' desaturation, are included in the plot. \emph{Tags}="all" plots all episodes present
#' in the data.
#' @param Indexes List of indexes of episodes (see \code{\link{ListEpisodes}})
#' to specify which episodes are included in the plot. 
#' \emph{Indexes}="all" plots all episodes present in the data.
#' @param main A main title for the plot.
#' @param xlab A label for the x axis.
#' @param ylab a label for the y axis
#' @param type 1-character string giving the type of plot desired. See 
#' \code{\link[graphics]{plot.default}}.
#' @param ylim The y limits of the plot.
#' @param verbose  Deprecated argument maintained for compatibility, 
#' use SetVerbose() instead 
#' @param Tag Deprecated argument maintained for
#' compatibility, use \emph{Tags} instead.
#' @param ... Other graphical parameters. See 
#' \code{\link[graphics]{plot.default}}.
#' @references L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri,
#' X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate
#' variability spectral analysis", Computer Methods and Programs in
#' Biomedicine 103(1):39-50, july 2011.
#' @author M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila, C.A. Garcia
#' @keywords aplot
PlotHR <-
  function(HRVData, Tags=NULL, Indexes=NULL,
           main = "Interpolated instantaneous heart rate",
           xlab="time (sec.)", ylab="HR (beats/min.)",type="l",ylim=NULL,
           Tag=NULL, verbose=NULL, ...) {
    # -----------------------------
    # Plots interpolated Heart Rate
    # -----------------------------
    #	Tags -> Tags of episodes to include in the plot
    #    "all" includes all types
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    Tags = HandleDeprecatedTagArgument(Tag = Tag, Tags = Tags)
    CheckInterpolation(HRVData)
    Tags = CheckTags(Tags, HRVData$Episodes)
    Indexes = CheckIndexes(Indexes, HRVData$Episodes)
    
    VerboseMessage(HRVData$Verbose, 	
                   "Plotting interpolated instantaneous heart rate")
    
    npoints = length(HRVData$HR)
    VerboseMessage(HRVData$Verbose, 	
                   paste("Number of points:", npoints))
    
    first = head(HRVData$Beat$Time,1)
    last = tail(HRVData$Beat$Time,1)
    x = seq(first, last, length.out = npoints)
    
    HRMin = min(HRVData$HR)
    HRMax = max(HRVData$HR)
    HRDiff = HRMax - HRMin
    
    
    if (is.null(ylim)) {
      ylim = c(HRMin - 0.1 * HRDiff, HRMax)
    }
    plot(x, HRVData$HR, type = type, xlab = xlab, ylab = ylab,
         ylim = ylim, ...)
    grid()
    
    if (!is.null(Tags) || !is.null(Indexes)) {
      # Data for representing episodes
      EpisodesToPlot <- selectEpisodes(HRVData$Episodes,Tags,Indexes)
      EpisodesToPlot <- EpisodesToPlot[EpisodesToPlot$selected,]
      
      # Data for representing episodes
      EpisodesAuxLeft <- EpisodesToPlot$InitTime
      EpisodesAuxBottom <- c(HRMin-0.09*HRDiff,HRMin-0.04*HRDiff)
      EpisodesAuxRight <- EpisodesToPlot$InitTime + EpisodesToPlot$Duration
      EpisodesAuxTop <- c(HRMin-0.07*HRDiff,HRMin-0.02*HRDiff)
      EpisodesAuxType <- EpisodesToPlot$Type
      
      labels <- levels(factor(EpisodesAuxType))
      
      Pal=rainbow(length(labels))
      Bor=Pal[match(EpisodesAuxType,labels)]
      
      VerboseMessage(HRVData$Verbose, 	
                     paste("No of episodes:",length(EpisodesAuxLeft)))
      VerboseMessage(HRVData$Verbose, 	
                     paste("No of classes of episodes:", length(Pal)))
      
      if (length(EpisodesAuxLeft)==1) {
        rect(EpisodesAuxLeft,EpisodesAuxBottom[1],EpisodesAuxRight,EpisodesAuxTop[1],border=Bor,col=Bor)
      } else {
        rect(EpisodesAuxLeft,EpisodesAuxBottom,EpisodesAuxRight,EpisodesAuxTop,border=Bor,col=Bor)
      }
      
      for (i in 1:length(EpisodesAuxLeft)) {
        lines(rep(EpisodesAuxLeft[i],times=2),c(HRMin-0.1*HRDiff,HRMax),lty=2,col=Bor[i])
        lines(rep(EpisodesAuxRight[i],times=2),c(HRMin-0.1*HRDiff,HRMax),lty=2,col=Bor[i])
      }
      
      legend("topright",inset=0.01,legend=labels,fill=Pal,cex=0.6,horiz=FALSE,bg='white')
    }
    
    title(main=main)
    
  }

