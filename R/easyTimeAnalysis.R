# Creating time analysis data frames
# @importFrom RHRV CreateTimeAnalysis
# @importFrom foreach foreach
# @importFrom foreach %dopar% %do%
easyTimeAnalysis <-
  function(format, files, groups, paths, easyOptions, ...) {
    # opts <- NULL
    # if (easyOptions$verbose) {
    #   opts <- list("progress" = updateProgressFactory("Time analysis", files))
    # }
    dataFrame <- suppressPackageStartupMessages({
      foreach(
        file = files,
        itcounter = seq_along(files),
        group = groups,
        path = paths,
        .combine = rbind.data.frame,
        #.export = c("prepareAnalysis", "easyCall"),
        # .packages = "RHRV",
        # .options.snow = opts,
        .errorhandling = "pass"
      ) %dopar% {
        hrv.data <- prepareAnalysis(file = file, rrs = path, format = format,
                                      easyOptions = easyOptions)
        hrv.data <- easyCall(hrv.data, CreateTimeAnalysis, ...)
        results <- hrv.data$TimeAnalysis[[1]]
        results$size <- NULL
        rowList <- c(
          list("file" = file),
          list("group" = group),
          results
        )
        # if (easyOptions$verbose && !easyOptions$parallel) {
        #   opts$progress(itcounter)
        # }
        if (easyOptions$verbose) {
          message(paste("Time analysis of", file, "done"))
        }
        as.data.frame(rowList)
      }
    })
    dataFrame
  }
