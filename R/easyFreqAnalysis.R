
# @importFrom foreach foreach
# @importFrom stats median
easyFreqAnalysis <-
  function(format, files, groups, paths, easyOptions, ...) {
    # opts <- NULL
    # if (easyOptions$verbose) {
    #   opts <- list("progress" = updateProgressFactory("Freq analysis", files))
    # }
    dataFrame <- foreach(
      file = files,
      itcounter = seq_along(files),
      group = groups,
      path = paths,
      .combine = rbind.data.frame,
      .export = c("prepareAnalysis", "easyCall"),
      # .packages = "RHRV",
      #.options.snow = opts,
      .errorhandling = "pass"
    ) %dopar% {
      hrv.data <- prepareAnalysis(file = file, rrs = path, format = format,
                                    easyOptions = easyOptions)
      hrv.data <- withCallingHandlers(
        {
          easyCall(hrv.data, InterpolateNIHR, ...)
        },
        warning = function(w) {
          w$message <- paste0("File ", file,": ", w$message, "\n")
          warning(w)
          invokeRestart("muffleWarning")
        }
      )
      zeroIndices <- which(hrv.data$HR == 0)
      hrMedian <- median(hrv.data$HR[-zeroIndices])
      hrv.data$HR[zeroIndices] <- hrMedian
      hrv.data <- easyCall(hrv.data, CreateFreqAnalysis, ...)
      hrv.data <- easyCall(hrv.data, CalculatePSD, doPlot = F, ...)
      x1 <- easyCall(hrv.data, CalculateEnergyInPSDBands, ...)
      names(x1) <- c("ULF", "VLF", "LF", "HF")
      rowList <- c(list("file" = file), x1, list("group" = group))
      # if (easyOptions$verbose && !easyOptions$parallel) {
      #   opts$progress(itcounter)
      # }
      if (easyOptions$verbose) {
        message(paste("Freq. analysis of", file, "done"))
      }
      as.data.frame(rowList)
    }
    dataFrame
  }

# @importFrom foreach foreach
# @importFrom stats median
easyWaveletAnalysis <-
  function(format, files, groups, paths, easyOptions, ...) {
    # opts <- NULL
    # if (easyOptions$verbose) {
    #   opts <- list("progress" = updateProgressFactory("Wavelet analysis", files))
    # }
    freqResults <- foreach(
      file = files,
      itcounter = seq_along(files),
      group = groups,
      path = paths,
      .combine = rbind.data.frame,
      .export = c("prepareAnalysis", "easyCall"),
      # .packages = "RHRV",
      # .options.snow = opts,
      .errorhandling = "pass"
    ) %dopar% {
      hrv.data <- prepareAnalysis(file = file, rrs = path, format = format, easyOptions = easyOptions)
      hrv.data <- withCallingHandlers(
        {
          easyCall(hrv.data, InterpolateNIHR, ...)
        },
        warning = function(w) {
          w$message <- paste0("File ", file,": ", w$message, "\n")
          warning(w)
          invokeRestart("muffleWarning")
        }
      )
      zeroIndices <- which(hrv.data$HR == 0)
      hrMedian <- median(hrv.data$HR[-zeroIndices])
      hrv.data$HR[zeroIndices] <- hrMedian

      hrv.data <- easyCall(hrv.data, CreateFreqAnalysis, ...)
      hrv.data <- SetVerbose(hrv.data, FALSE) # Set to False to avoid clutter
      hrv.data <- easyCall(hrv.data, CalculatePowerBand, ...)

      index <- length(hrv.data$FreqAnalysis)
      resultsWavelet <- hrv.data$FreqAnalysis[[index]]
      resultsWavelet$file <- file
      resultsWavelet$ULF <- sum(hrv.data$FreqAnalysis[[index]]$ULF)
      resultsWavelet$VLF <- sum(hrv.data$FreqAnalysis[[index]]$VLF)
      resultsWavelet$LF <- sum(hrv.data$FreqAnalysis[[index]]$LF)
      resultsWavelet$HF <- sum(hrv.data$FreqAnalysis[[index]]$HF)
      resultsWavelet[
        c("HRV", "LFHF", "Time", "wavelet", "bandtolerance", "depth", "type",
          paste0(c("ULF", "VLF", "LF", "HF"), rep(c("min", "max"), each = 4))
        )
      ] <- NULL
      rowList <- c(resultsWavelet, list("group" = group))
      # if (easyOptions$verbose && !easyOptions$parallel) {
      #   opts$progress(itcounter)
      # }
      if (easyOptions$verbose) {
        message(paste("Freq. analysis of", file, "done"))
      }
      as.data.frame(rowList)
    }
    freqResults
  }
