
getIndexValuesInRange = function(x, rang){
  which( (rang[[1]] <= x) & (x <= rang[[2]]) )
}


VerboseMessage = function(verbose, msg, symbol="", tab=""){
  if (verbose) {
    message(paste0(tab, symbol, " ", msg, " ", symbol))
  }
}

# If new is NULL -> there is no new argument replacing the old one, we ignore
# the argument. In other case we recommend to use the new one
DeprecatedArgMessage = function(old, new = NULL) {
  if (is.null(new)) {
    return(paste0(old, ": deprecated argument ignored."))
  } else{
    return(paste0(old, ": deprecated argument. Using ",new," instead."))
  }
}

MissingNonLinearObjectMessage = function(missing, use_before, use_after) {
  paste0(missing," not found! Use ",
         use_before, " before using ",
         use_after,"!")
}

# format numbers for VerboseMessage
rhrvFormat = function(x){
  kNDIGITS = 4
  round(x, digits = kNDIGITS)
}


easyFileValidation <- function(path, easyOptions) {
  # 1. Check if path really exists
  if (dir.exists(path) != TRUE) {
    stop("The path ", path, " does not exist")
  }
  # 2. The path contains files:
  if ((length(list.files(path)) > 0) != TRUE) {
    stop(
      paste0(
        "The path ", path, " exists but there are no files in it"
      )
    )
  } else {
    if (easyOptions$verbose) {
      message(
        "The path ", path, " exists and there are files in it"
      )
    }
  }
}


prepareAnalysis <- function(file, rrs, format, easyOptions) {
  hrv.data <- CreateHRVData()
  hrv.data <- SetVerbose(hrv.data, FALSE)

  hrv.data <- tryCatch({
    hrv.data <- LoadBeat(
      fileType = format,
      HRVData = hrv.data,
      Recordname = file,
      RecordPath = rrs
    )
    hrv.data
  },
  error = function(cond) {
    stop(
      paste(
        "The file \"",
        file,
        "\" could not be loaded. Check if the file is in the correct format; the specified format was \"",
        format,
        "\".",
        sep = ""
      )
    )
  })
  hrv.data <- withCallingHandlers(
    {
      hrv.data <- BuildNIHR(hrv.data)
      hrv.data <- FilterNIHR(hrv.data)
      hrv.data$Beat <- hrv.data$Beat[2:nrow(hrv.data$Beat), ]
      hrv.data
    },
    warning = function(w) {
      w$message <- paste0("File ", file,": ", w$message, "\n")
      warning(w)
      invokeRestart("muffleWarning")
    }
  )
  hrv.data
}

#Calls an RHRV function with hrv.data after cleaning the parameters
# @importFrom plotrix clean.args
easyCall <- function(hrv.data, mf, ...) {
  args.list <- plotrix::clean.args(list(...), mf)
  args.list$HRVData <- hrv.data
  do.call(mf, args.list)
}

# @importFrom parallel detectCores
# @importFrom parallel makeCluster
# @importFrom doParallel registerDoParallel
prepareEasyCluster <- function(nJobs, verbose) {
  nCores <- parallel::detectCores(logical = FALSE)
  if (nJobs <= 0) {
    nJobs <- nCores
  } else if (nJobs > nCores) {
    nJobs <- nCores
  }
  if (nJobs > 1) {
    cl <- parallel::makeCluster(nJobs, outfile="") # using outfile = "" may be useful for debugging
    # doSNOW::registerDoSNOW(cl) # superseded by doParallel, although it permits progress bar
    doParallel::registerDoParallel(cl)
    if (verbose) {
      message(paste("Registering cluster with", nJobs, "nodes"))
    }
  } else {
    cl <- NULL
    foreach::registerDoSEQ()
  }
  cl
}


splitPath <- function(path) {
  if (dirname(path) %in% c(".", path))
    return(basename(path))
  return(c(basename(path), splitPath(dirname(path))))
}


# FIXME @importClassesFrom progress progress_bar
# @import progress
# updateProgressFactory <- function(analysis, files){
#   pb <- progress::progress_bar$new(
#     format = paste(analysis, "of :file [:bar] :elapsed | eta: :eta"),
#     total = length(files),
#     width = 80
#   )
#   function(n) {
#     pb$tick(tokens = list("file" = files[n]))
#   }
# }
