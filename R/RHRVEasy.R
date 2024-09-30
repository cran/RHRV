#' Run a full HRV analysis including indices computation and statistical analysis
#'
#' @param folders A character vector with the paths to the folders containing
#' the HRV files. Each folder should contain the HRV files of a group.
#' @param correctionMethod The method to correct for multiple comparisons. Can
#' be one of "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr" and
#' "none". Default is "bonferroni".
#' @param verbose Logical. If TRUE, the function will show a progress bar and
#' print additional information to the console.
#' @param format The format of the HRV files. Can be one of "WFDB", "Ascii",
#' "RR", "Polar", "Suunto", "EDFPlus" and "Ambit".
#' @param typeAnalysis The type of frequency analysis to perform. Can be one of
#' "fourier" or "wavelet".
#' @param significance The significance level to use in the statistical
#' analysis. By default, it is set to 0.05.
#' @param nonLinear Logical. If TRUE, the function will compute non-linear
#' indices. It should be noted that this process is computationally expensive.
#' @param doRQA Logical. If TRUE, the function will compute Recurrence
#' Quantification Analysis (RQA) indices. This parameter is ignored if
#' `nonLinear` is set to FALSE.It should be noted that this process is
#' computationally expensive.
#' @param nJobs The number of parallel jobs to use. `nJobs <= 0` uses all cores
#' available. By default, it is set to 1.
#' @param saveHRVIndicesInPath The path where the HRV indices will be saved as
#' an excel file. If NULL, the indices will not be saved. See [SaveHRVIndices()]
#' for more details.
#' @param ... Additional arguments for the HRV analysis. For further details,
#' see the `RHRV` package.
#' @returns An object of class `RHRVEasyResult` containing the HRV indices
#' (slot `$HRVIndices`) and the statistical analysis results (slot `$stats`)
#' @export
RHRVEasy <-
  function(folders,
           correctionMethod = c(
             "bonferroni", "holm", "hochberg", "hommel", "BH",
             "BY", "fdr", "none"
           ),
           verbose = FALSE,
           format = "RR",
           typeAnalysis = c("fourier", "wavelet"),
           significance = 0.05,
           nonLinear = FALSE,
           doRQA = FALSE, # ignored if nonLinear = FALSE
           nJobs = 1,
           saveHRVIndicesInPath = NULL,
           ...) {
    typeAnalysis <- match.arg(typeAnalysis)
    correctionMethod <- match.arg(correctionMethod)
    easyOptions <- buildEasyOptions(
      verbose = verbose,
      significance = significance,
      method = correctionMethod
    )
    HRVIndices <- calculateHRVIndices(
      folders = folders,
      format = format,
      typeAnalysis = typeAnalysis,
      nonLinear = nonLinear,
      doRQA = doRQA,
      nJobs = nJobs,
      easyOptions = easyOptions,
      ...
    )
    tempfilename <- tempfile(pattern = "RHRVEasy", tmpdir = tempdir(), fileext = ".RDS")
    saveRDS(HRVIndices, file = tempfilename)
    message(
      paste0("Saving temporary HRV Indices. You may read them by using readRDS('", tempfilename, "')")
    )

    pVals <- statsAnalysis(HRVIndices, easyOptions)
    results <- RHRVEasyResult(HRVIndices, pVals, easyOptions)

    if (!is.null(saveHRVIndicesInPath)) {
      SaveHRVIndices(results, saveHRVIndicesInPath)
    }

    results
  }




# @importFrom tibble as_tibble
RHRVEasyResult <- function(HRVIndices, pVals, easyOptions) {
  HRVIndices <- as_tibble(HRVIndices)
  pVals <- as_tibble(pVals)
  # Ensure reasonable order in pVals
  desiredOrder <- c("HRVIndex", "method", "p.value", "adj.p.value")
  if ("pairwise" %in% colnames(pVals)) {
    desiredOrder <- c(desiredOrder, "pairwise")
  }
  pVals <- pVals[, desiredOrder]
  if ("pairwise" %in% colnames(pVals)) {
    pairwiseOrder <- c("HRVIndex", "group1", "group2", "method", "p.value", "adj.p.value")
    pVals$pairwise <- lapply(pVals$pairwise, function(x){
      if (!is.null(x)) {
        x <- as_tibble(x)
        x <- x[, pairwiseOrder]
      }
      x
    })
  }
  results <- list("HRVIndices" = HRVIndices, "stats" = pVals)
  class(results) <- "RHRVEasyResult"
  attr(results, "easyOptions") <- easyOptions
  results
}

#' Rerun the statistical analysis from a previous `RHRVEasy()` call
#'
#' @param RHRVEasyResultObject An object of class `RHRVEasyResult` as returned
#' by `RHRVEasy()`
#' @param correctionMethod The method to correct for multiple comparisons. Can
#' be one of "bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr" and
#' "none". Default is "bonferroni".
#' @param significance The significance level to use in the statistical
#' analysis. By default, it is set to 0.05.
#' @returns An object of class `RHRVEasyResult` containing the HRV indices
#' (slot `$HRVIndices`) and the statistical analysis results (slot `$stats`)
#' @seealso [RHRVEasy()]
#' @export
RHRVEasyStats <- function(RHRVEasyResultObject,
                          correctionMethod = c(
                            "bonferroni", "holm", "hochberg", "hommel", "BH",
                            "BY", "fdr", "none"
                          ),
                          significance = 0.05) {
  if (!inherits(RHRVEasyResultObject, "RHRVEasyResult")) {
    stop("RHRVEasyResultObject should be a 'RHRVEasyResult' object, as returned by 'RHRVEasy()'")
  }
  correctionMethod <- match.arg(correctionMethod)
  stopifnot((significance > 0) && (significance < 1))
  easyOptions <- attr(RHRVEasyResultObject, "easyOptions")
  easyOptions$method <- correctionMethod
  easyOptions$significance <- significance
  HRVIndices <- RHRVEasyResultObject$HRVIndices
  pVals <- statsAnalysis(HRVIndices, easyOptions)
  RHRVEasyResult(HRVIndices, pVals, easyOptions)
}


#' Save the HRV indices as an excel file
#'
#' @param RHRVEasyResultObject An object of class `RHRVEasyResult` as returned
#' by `RHRVEasy()`
#' @param saveHRVIndicesInPath The path where the HRV indices will be saved as
#' an excel file. The name of the file is automatically created based on the
#' groups being compared.
#' @param filename Filename of the excel file. If not provided, the name of the file
#' is built using the names of the groups being compared.
# @importFrom writexl write_xlsx
# @export
SaveHRVIndices <- function(RHRVEasyResultObject, saveHRVIndicesInPath = ".",
                           filename = NULL) {
  if (is.null(filename)) {
    filename <- paste0(
      paste(unique(RHRVEasyResultObject$HRVIndices$group), collapse = "_Vs_"),
      ".xlsx"
    )
  }
  tryCatch(
    {
      filename <- file.path(
        saveHRVIndicesInPath,
        filename
      )
      write_xlsx(RHRVEasyResultObject$HRVIndices, filename)
    },
    error = function(e) {
      message(paste0("Could not save indices in path '", saveHRVIndicesInPath, "'"))
      message(e)
    }
  )
}



# @importFrom boot boot.ci
# @importFrom boot boot
# @importFrom stats t.test
computeEasyCIs <- function(easyObject, test, confLevel) {
  groupedData <- split(easyObject$HRVIndices[[test$HRVIndex]], easyObject$HRVIndices$group)
  if (test$method == "ANOVA") {
    cis <- lapply(groupedData, \(x) {
      ci <- t.test(x, conf.level = confLevel)$conf.int
      attr(ci, "method") <- "Normal CI without adjustment"
      ci
    })
  } else {
    cis <- lapply(groupedData, \(x) {
      ci <- boot::boot.ci(
        boot::boot(x[!is.na(x)], statistic = \(x, i) mean(x[i]), R = 1000),
        type = "basic",
        conf = confLevel
      )
      ci <- ci$basic[, 4:5]
      attr(ci, "conf.level") <- confLevel
      attr(ci, "method") <- "Bootstrap CI without adjustment"
      ci
    })
  }
  cis
}

printGroupCI <- function(cis, group, digits, nspaces = 2) {
  conf <- attr(cis[[group]], "conf.level")
  method <- attr(cis[[group]], "method")
  cat(
    sep = "",
    rep(" ", nspaces), group, "'s mean", conf * 100, "% CI: (",
    round(cis[[group]][1], digits = digits), ", ",
    round(cis[[group]][2], digits = digits), ")",
    " [", method, "]\n"
  )
}


# @importFrom tibble as_tibble
# @importFrom utils capture.output
# @method print RHRVEasyResult
#' @export
print.RHRVEasyResult <- function(x, digits = getOption("digits"), ...) {
  firstLevelSpaces <- 2
  nPosthocSpaces <- 4
  easyOptions <- attr(x, "easyOptions")
  significantx <- x$stats[x$stats$adj.p.value < easyOptions$significance, ]
  if (is.null(significantx) || nrow(significantx) == 0) {
    cat(
      "No significant differences were found between groups\n"
    )
  } else {
    adjMethod <- ifelse(easyOptions$method == "none", "", easyOptions$method)
    for (sigRow in seq_len(nrow(significantx))) {
      test <- significantx[sigRow, ]
      # Compute mean CIS using Normal CIs or Boostrapped CIs depending on the
      # data distribution
      cis <- computeEasyCIs(x, test, confLevel = 1 - easyOptions$significance)
      # Actual printing happens here
      with(test, {
        cat(
          sep = "",
          "Significant differences in ", HRVIndex, " (", method, ", ",
          adjMethod, " p-value = ", format(adj.p.value, digits), "):\n"
        )
      })
      if (length(cis) == 2) {
        for (group in names(cis)) {
          printGroupCI(cis, group, digits, nspaces = firstLevelSpaces)
        }
      } else {
        method <- test$pairwise[[1]]$method[[1]]
        isSignificantPosthoc <- test$pairwise[[1]]$adj.p.value < easyOptions$significance
        if (!any(isSignificantPosthoc)) {
          cat(
            rep(" ", firstLevelSpaces),
            "No significant differences were found between groups in post-hoc tests (",
            method, " + ", adjMethod, "-p-value adjustment).\n"
          )
        } else {
          cat(
            sep = "",
            rep(" ", firstLevelSpaces),
            "Significant differences in the post-hoc tests (",
            method, " + ", adjMethod, "-p-value adjustment):\n"
          )

          significantPosthocs <- test$pairwise[[1]][isSignificantPosthoc, ]
          significantPosthocs <- significantPosthocs[order(significantPosthocs$group1), ]
          posthocTable <- capture.output(
            as_tibble(significantPosthocs[, c("group1", "group2", "adj.p.value")])
          )
          # eliminate tibble head and type info
          posthocTable <- posthocTable[-c(1, 3)]
          maxLen <- max(sapply(posthocTable, nchar))
          printSpaces <- paste0(rep(" ", nPosthocSpaces), collapse = "")
          posthocTable <- sapply(posthocTable, \(x) paste0(printSpaces, x))
          cat(paste0(posthocTable, collapse = "\n"))
          cat("\n", printSpaces, rep("-", maxLen), "\n", sep = "")
          for (group in sort(names(cis))) {
            printGroupCI(cis, group, digits, nspaces = nPosthocSpaces)
          }
        }
      }
      cat("\n")
    }
  }
  invisible(x)
}


# @importFrom stats p.adjust
buildEasyOptions <- function(verbose, significance, method) {
  # fake call to p.adjust to check for the method name
  invisible(p.adjust(rep(0.1, 3), method = method))
  stopifnot((significance > 0) && (significance < 1))
  list(
    "verbose" = verbose,
    "significance" = significance,
    "method" = method
  )
}


calculateHRVIndices <- function(
    folders,
    format,
    typeAnalysis,
    nonLinear,
    doRQA, # ignored if nonLinear = FALSE
    nJobs,
    easyOptions,
    ...) {
  cl <- prepareEasyCluster(nJobs, easyOptions$verbose)
  easyOptions$parallel <- !is.null(cl)

  filesByFolder <- lapply(folders, \(folder) {
    easyFileValidation(folder, easyOptions)
    data.frame(
      "folder" = folder,
      "file" = list.files(folder),
      "group" = splitPath(folder)[[1]]
    )
  })
  files <- do.call(rbind, filesByFolder)
  files$group <- factor(files$group)

  timeResults <- easyTimeAnalysis(
    format = format,
    files = files$file,
    groups = files$group,
    paths = files$folder,
    easyOptions = easyOptions,
    ...
  )
  # Frequency analysis
  if (typeAnalysis == "fourier") {
    freqResults <- easyFreqAnalysis(
      format = format,
      files = files$file,
      groups = files$group,
      paths = files$folder,
      easyOptions = easyOptions,
      ...
    )
  }
  if (typeAnalysis == "wavelet") {
    freqResults <-
      easyWaveletAnalysis(
        format = format,
        files = files$file,
        groups = files$group,
        paths = files$folder,
        type = typeAnalysis,
        easyOptions = easyOptions,
        ...
      )
  }
  if (nonLinear) {
    nonlinearResults <-
      easyNonLinearAnalysis(
        format = format,
        files = files$file,
        groups = files$group,
        paths = files$folder,
        easyOptions = easyOptions,
        doRQA = doRQA,
        ...
      )
  }
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }

  # Merge
  allResults <- merge(
    timeResults,
    freqResults,
    by = c("file", "group"),
    all = TRUE
  )
  if (nonLinear) {
    allResults <- merge(
      allResults,
      nonlinearResults,
      by = c("file", "group"),
      all = TRUE
    )
  }
  allResults
}
