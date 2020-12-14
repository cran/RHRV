EditNIHR <-
  function(HRVData,scale=1.0, verbose=NULL) {
    #---------------------------------------
    # Edits beats interactively
    #	Requires tcltk and tkrplot libraries
    #---------------------------------------
    #	Plots Non-interpolated instantaneous heart rate for manual removing of outliers
    #	scale -> allow scaling for small screens

    if (!requireNamespace("tkrplot", quietly = TRUE)) {
      stop("Package \"tkrplot\" needed for this function to work. Please install it.",
        call. = FALSE)
    }


    if (!requireNamespace("tcltk", quietly = TRUE)) {
      stop("Package \"tcltk\" needed for this function to work. Please install it.",
        call. = FALSE)
    }

    
    
    HRVData = HandleVerboseArgument(HRVData, verbose)
    VerboseMessage(HRVData$Verbose,
                   "Manually editing non-interpolated instantaneous heart rate")
    CheckBeats(HRVData)
    CheckNIHR(HRVData)
    
    editFunction <- function(HRVData) {
      
      HRVDataOld <- HRVData
      
      Myhscale <- 2*scale    # Horizontal scaling
      Myvscale <- 1.5*scale    # Vertical scaling
      
      plt <- c()
      usr <- c()
      coords <- c()
      pointsInArea <- c()
      numPointsInArea <- 0
      numCoords <- 0
      numRemovedPoints <- 0
      vectorx <- c()
      vectory <- c()
      
      
      plotFunction <- function()
      {
        vectorx <<- HRVData$Beat$Time
        vectory <<- HRVData$Beat$niHR
        plot(vectorx,vectory, type = "l", xlab = "time (sec.)",
             ylab = "HR (beats/min.)",
             ylim = c(min(vectory),max(vectory)*1.1))
        title(main = "Non-interpolated instantaneous heart rate")
        
        if (numCoords == 1) {
          points(coords[1],coords[2], pch = "+", col = "red")
        }
        if (numCoords == 2) {
          rect(min(coords[1],coords[3]),min(coords[2],coords[4]),
               max(coords[1],coords[3]),max(coords[2],coords[4]),border="red")
          areaString=paste("No. of selected points: ",numPointsInArea)
          text((coords[1]+coords[3])/2,max(coords[2],coords[4]),areaString,pos=3,col="red")
          points(vectorx[pointsInArea==TRUE],vectory[pointsInArea==TRUE],pch=20,col="red")
          
        }
        
        usr <<- par('usr')
        plt <<- par('plt')		
      }
      
      tt <- tcltk::tktoplevel()
      tcltk::tkwm.deiconify(tt)
      tcltk::tkgrab.set(tt)
      tcltk::tkfocus(tt)
      tcltk::tkwm.title(tt,"Outliers removal")
      img <- tkrplot::tkrplot(tt,fun=plotFunction,hscale=Myhscale,vscale=Myvscale)
      
      
      Remove <- function()
      {
        numCoords <<-0
        coords <<- c()
        HRVData$Beat <<- subset(HRVData$Beat, pointsInArea==FALSE)
        numRemovedPoints <<- numRemovedPoints + numPointsInArea
        VerboseMessage(HRVData$Verbose,
                       paste0("Removing ", numPointsInArea, 
                              " points (",numRemovedPoints," so far)")
        )
        
        pointsInArea <<- c()
        numPointsInArea <<- 0
        tkrplot::tkrreplot(img)
      }
      
      Clear <- function() {
        numCoords <<- 0
        coords <<- c()
        pointsInArea <<- c()
        numPointsInArea <<- 0
        VerboseMessage(HRVData$Verbose, "Clearing point selection")
        tkrplot::tkrreplot(img)
      }
      
      Quit <- function() {
        VerboseMessage(HRVData$Verbose, "Manual edition ended... quitting")
        
        if (numRemovedPoints > 0) {
          msg <- paste(numRemovedPoints,"outliers to be removed\nProceed?")
          mbval <- tcltk::tkmessageBox(title = "Confirmation", message = msg,
                                type = "yesnocancel", icon = "question")
          
          if (tcltk::tclvalue(mbval) == "no") {
            tcltk::tkgrab.release(tt)
            tcltk::tkdestroy(tt)
            HRVData <<- HRVDataOld
          }
          if (tcltk::tclvalue(mbval) == "yes") {
            tcltk::tkgrab.release(tt)
            tcltk::tkdestroy(tt)
          }		
        } else {
          tcltk::tkdestroy(tt)
        }
        
        
      }
      
      OnLeftClick <- function(x,y) {
        
        xClick <- as.numeric(x)
        yClick <- as.numeric(y)
        width  <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqwidth",img)))
        height <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqheight",img)))
        #message("Width:",width,"\n")
        #message("Height:",height,"\n")
        
        xMin <- plt[1] * width
        xMax <- plt[2] * width
        yMin <- plt[3] * height
        yMax <- plt[4] * height
        #message("xMin:",xMin,"\n")
        #message("xMax:",xMax,"\n")
        #message("yMin:",yMin,"\n")
        #message("yMax:",yMax,"\n")		
        
        rangeX <- usr[2] - usr[1]
        rangeY <- usr[4] - usr[3]
        #message("X range:",rangeX,"\n")
        #message("Y range:",rangeY,"\n")
        
        xCoord <- usr[1] + (xClick - xMin) * rangeX / (xMax - xMin)
        yCoord <- usr[3] + ((height - yClick) - yMin) * rangeY / (yMax - yMin)
        
        VerboseMessage(HRVData$Verbose, 
                       paste0("Point clicked: (", xCoord,", ", yCoord,")"))
        
        if ((xClick > xMin * 0.95) && (xClick < xMax * 1.1) && 
            (yClick > yMin * 0.95) && (yClick < yMax * 1.1)) {
          if (numCoords == 0) {
            numCoords <<- 1
            #message("Primer punto\n")
            coords <<- c(xCoord,yCoord)
          } else if (numCoords == 1) {
            coords <<- c(coords,xCoord,yCoord)
            numCoords <<- 2
            pointsInArea <<- (
              (vectorx > min(coords[1], coords[3])) &
                (vectorx < max(coords[1], coords[3])) &
                (vectory > min(coords[2], coords[4])) &
                (vectory < max(coords[2], coords[4]))
            )
            numPointsInArea <<- length(pointsInArea[pointsInArea==TRUE])
            
            VerboseMessage(HRVData$Verbose,
                           paste(numPointsInArea, "points found in area"))
          }
          tkrplot::tkrreplot(img)
        }
      }
      
      buttonremove <- tcltk::tkbutton(tt, text = "Remove outliers", command = Remove)
      buttonclear <- tcltk::tkbutton(tt, text = "Clear", command = Clear)
      buttonremove2 <- tcltk::tkbutton(tt, text = "End", command = Quit)
      
      
      tcltk::tkgrid(img, columnspan = 3)
      tcltk::tkgrid(buttonremove,buttonclear,buttonremove2)
      tcltk::tkbind(img, "<Button-1>",OnLeftClick)
      tcltk::tkwait.window(tt)
      
      return(HRVData)
    }
    
    HRVDataNew = editFunction(HRVData)
    return(HRVDataNew)
  }

