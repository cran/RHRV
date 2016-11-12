CheckAnalysisIndex <- function(index, numberOfIndex, type = c("frequency", "nonlinear")) {
  
  type = match.arg(type)
  useName = switch(type,
                   frequency = "frequency",
                   nonlinear = "non-linear")
  
  useMethod = switch(type,
                     frequency = "CreateFreqAnalysis()",
                     nonlinear = "CreateNonLinearAnalysis()")
  
  useIndexName = switch(type,
                        frequency = "indexFreqAnalysis",
                        nonlinear = "indexNonLinearAnalysis")
  
  if (numberOfIndex < 1 )  {
    msg = paste("There are no",useName,"analysis structures!\nCreate some using",useMethod,"---")
    stop(msg)
  }
  
  if (index < 1)  {
    msg = paste("Invalid",useIndexName,index)
    stop(msg)
  }
  
  if (numberOfIndex < index)  {
    msg = paste(useIndexName,"analysis no.",index," not present!")
    stop(msg)
  }
}


CheckDeprecatedArg = function(old, new = NULL) {
  if (!is.null(old)) {
    warning(DeprecatedArgMessage(deparse(substitute(old)), new))
  }  
}

HandleVerboseArgument = function(HRVData, verbose) {
  if (!is.null(verbose)) {
    warning(DeprecatedArgMessage("verbose", "SetVerbose()"))
    HRVData = SetVerbose(HRVData, verbose)
  }
  HRVData  
}

HandleDeprecatedTagArgument = function(Tag, Tags) {
  if (is.null(Tags) && !is.null(Tag)) {
    warning(DeprecatedArgMessage("Tag", "Tags"))
    Tags <- Tag
  } 
  return(Tags)
}

# Check if some Beats have been loaded
CheckBeats = function(HRVData) {
  if (is.null(HRVData$Beat)) {
    stop("RR intervals have not been loaded! Use the LoadBeat() function")
  }  
} 

# Check if the BuildNIHR has been called 
CheckNIHR = function(HRVData) {
  CheckBeats(HRVData)
  if (is.null(HRVData$Beat$RR)) {
    stop("RR intervals have not been calculated!\nUse the BuildNIHR() function")
  }  
}

# Check if interpolation has been performed 
CheckInterpolation = function(HRVData) {
  CheckNIHR(HRVData)
  if (is.null(HRVData$HR)) {
    stop("Frequency analysis needs interpolated data! Use the InterpolateNIHR() function")
  }  
}

# Check if the periodogram has been computed
CheckPeriodogram =  function(HRVData, indexFreqAnalysis) {
  if (is.null(HRVData$FreqAnalysis[[indexFreqAnalysis]]$periodogram)) {
    stop("PSD not found! Use the CalculatePSD() function")
  }
}

# Check if the spectrogram has been computed
CheckPowerBand =  function(HRVData, indexFreqAnalysis) {
  if (is.null(HRVData$FreqAnalysis[[indexFreqAnalysis]]$LF)) {
    stop("Spectrogram not found! Use the CalculatePowerBand() function")
  }
}

CheckEpisodes = function(HRVData) {
  if (is.null(HRVData$Episodes)) {
    stop("Episodes not present!")
  }
}

CheckNonLinearComputations = function(HRVData, index, statisticName, errorMsg) {
  if (is.null((HRVData$NonLinearAnalysis[[index]])[[statisticName]]$computations)) {
    stop(errorMsg)
  }
}

#  Function to check the correct Use of Tags/Indexes,  returning a new vector of
#  proper Tags/Indexes. We may distinguish four cases.
#  1.- Tags/Indexes not used and there are no Episodes: do nothing 
#  2.- Tags/Indexes not used, and there are Episodes: do nothing
#  3.- Tags/Indexes are used but there are no Episodes: throw a warning and 
#  return an empty list of Tags/Indexes
#  4.- Tags/Indexes are used and there are Episodes: return a list with only
#  the correct Tags/Indexes. Throw a warning if some of the Episodes/Indexes
#  were wrong.
#  
#  The function works with both Tags or Indexes (called in the function
#  episodesReferences, for the sake of generality). The validity of an 
#  episodeReference is checked using the function 
#  functionIsValid(episodesReferences, Episodes). argName is used
#  to print warning messages and should be "Tags" or "Indexes"
CheckTagsOrIndexes = function(episodesReferences, Episodes, 
                              functionIsValid, argName = c("Tags","Indexes")) {
  argName = match.arg(argName)
  # cases 1 and 2
  if (is.null(episodesReferences)) {
    return(NULL)
  }
  if (is.null(Episodes)) {
    warning("Episodes not present. Ignoring ", argName, "argument")
    episodesReferences = NULL
  }  else {
    # Episodes are not NULL from now on
    if (is.null(episodesReferences)) {
      return(episodesReferences)
    }
    if ("all" %in% episodesReferences) {
      if (length(episodesReferences) > 1) {
          warning("Setting '",argName," = \"all\"' and ignoring the remaining ", argName)
      }
      return(c("all"))
    }
    # get which episodesReferences are valid
    isValid = functionIsValid(episodesReferences, Episodes)
    # show warning if there are some invalid episodesReferences
    if (any(!isValid)) {
      if (sum(!isValid) == 1) {
        # use phrase in singular
        warning(paste(episodesReferences[!isValid], collapse = ", "),
                ": wrong ", ifelse(argName == "Tags", "Tag", "Index"),
                ". Ignoring it.")
      } else {
        # use phrase in plural
        warning(paste(episodesReferences[!isValid], collapse = ", "),
                ": wrong ", argName,
                ". Ignoring them.")
      }
    }
    if (sum(isValid) == 0) {
      return(NULL)
    } else {
      return(episodesReferences[isValid])
    }
  }
}

CheckTags = function(Tags, Episodes) {
  return(
    CheckTagsOrIndexes(Tags, Episodes, function(x, y){x %in% y$Type}, "Tags")
  )
}

CheckIndexes = function(Indexes, Episodes) {
  return(
    CheckTagsOrIndexes(Indexes, Episodes, function(ind, ep){
      # x and y are not NULL so we don't check it
      nEpisodes = nrow(ep)
      sapply(ind, function(x) {x >= 1 && x <= nEpisodes})
    }, "Indexes")
  )
}
