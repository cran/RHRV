#tryShapiroTest <- function(x) {
#  tryCatch({
#    shapiro.test(x)$p.value
#  }, error = function(e) {
#    # shapiro may fail if we have few data or all indices are equal.
#    # Let's assume normality returning a pval of 1
#    1
#   })
#}


# @importFrom stats shapiro.test
# @importFrom stats ks.test
tryNormalityCheck <- function(x) {
  pval <- tryCatch({
    if (length(x) >= 5000) {
      normalityCheck <- suppressWarnings(stats::ks.test(x, y = "pnorm", alternative = "two.sided"))
    } else {
      normalityCheck <- stats::shapiro.test(x)
    }
    normalityCheck$p.value
  },
  error = function(e) {
    warning(paste0(
      "Could not check normality of residuals. Assuming normality does not hold. (", e$message, ")"
    ))
    pval <- 0
    pval
  })
  pval
}


# @importFrom broom tidy
# @importFrom stats formula aov rstandard kruskal.test
columnStats <- function(HRVIndices, column, easyOptions) {
  analysisFormula <- formula(paste(column, "~ group"))
  statsResults <- aov(analysisFormula, data = HRVIndices)
  normalityPVal <- tryNormalityCheck(rstandard(statsResults))
  if (normalityPVal < easyOptions$significance) {
    # Normality does not hold: redo analysis
    statsResults <- kruskal.test(analysisFormula, data = HRVIndices)
    statsResults <- broom::tidy(statsResults)
  } else {
    # Normal data
    statsResults <- broom::tidy(statsResults)
    statsResults <- statsResults[statsResults$term == "group", ]
    statsResults$method <- "ANOVA"
  }
  statsResults <- statsResults[, c("p.value", "method")]
  statsResults$HRVIndex <- column
  statsResults
}

# @importFrom PMCMRplus kwAllPairsDunnTest
# @importFrom tidyr pivot_longer
# @importFrom tidyr all_of
# @importFrom broom tidy
#  @importFrom stats pairwise.t.test
indexPairwiseComparison <- function(HRVIndexValues, group, omnibusMethod) {
  if (omnibusMethod == "ANOVA") {
    pairwiseComp <- pairwise.t.test(HRVIndexValues, group, p.adjust.method = "none")
    tidyComp <- broom::tidy(pairwiseComp)
    tidyComp$method <- pairwiseComp$method
  } else {
    pairwiseComp <- PMCMRplus::kwAllPairsDunnTest(HRVIndexValues, group, p.adjust.method = "none")
    # No tidy method for PMCMR
    tidyComp <- as.data.frame(pairwiseComp$p.value)
    groups2 <- colnames(tidyComp)
    tidyComp$group1 <- rownames(tidyComp)
    tidyComp <- tidyr::pivot_longer(tidyComp, names_to = "group2", values_to = "p.value",
                                   tidyr::all_of(groups2))
    # filter NAs (self-comparisons, i.e., A Vs A, B Vs B, etc.)
    tidyComp <- tidyComp[!is.na(tidyComp$p.value), ]
    tidyComp$method <- pairwiseComp$method
  }
  tidyComp
}


# @importFrom iterators iter
# @importFrom stats p.adjust
statsAnalysis <- function(HRVIndices, easyOptions) {
  indicesNames <- setdiff(colnames(HRVIndices), c("file", "group", "timeLag", "embeddingDim"))
  pVals <- foreach(
    column = indicesNames,
    .combine = rbind
  ) %do% {
    columnStats(HRVIndices, column, easyOptions)
  }
  pVals$adj.p.value <- p.adjust(pVals$p.value, method = easyOptions$method)
  numberGroups <- length(unique(HRVIndices$group))
  # Do post-hoc if numberGroups > 2
  if (numberGroups > 2) {
    pairwiseComparisonsList <- foreach(indexTest = iterators::iter(pVals, by = "row")) %do% {
      pairwiseComp <- NULL
      if (indexTest$adj.p.value < easyOptions$significance) {
        pairwiseComp <- indexPairwiseComparison(
          HRVIndices[[indexTest$HRVIndex]],
          HRVIndices[["group"]],
          indexTest$method
        )
        pairwiseComp$HRVIndex <- indexTest$HRVIndex
      }
      pairwiseComp
    }
    pairwiseComparisons <- do.call(rbind, pairwiseComparisonsList)
    if (!is.null(pairwiseComparisons)) {
      pairwiseComparisons$adj.p.value <- p.adjust(pairwiseComparisons$p.value, method = easyOptions$method)
      splittedComparisons <- split(pairwiseComparisons, pairwiseComparisons$HRVIndex)
      pVals$pairwise <- lapply(pVals$HRVIndex, \(HRVIndex) {
        splittedComparisons[[HRVIndex]]
      })
    } else {
      pVals$pairwise <- pairwiseComparisonsList # This is a list of NULLs with the proper length
    }
  }
  pVals
}
