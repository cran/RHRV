## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

## ----eval=FALSE---------------------------------------------------------------
#  library("RHRV")
#  
#  basePath <- "book_data"  # adjust as needed
#  NSR_DB <- file.path(basePath, "normal")
#  CHF_DB <- file.path(basePath, "chf")
#  HEALTHY_DB <- file.path(basePath, "healthy")

## ----eval=FALSE---------------------------------------------------------------
#  spreadsheetPath <- basePath

## ----eval=FALSE, results=FALSE------------------------------------------------
#  easyAnalysis <- RHRVEasy(folders = c(NSR_DB, CHF_DB), nJobs = -1)

## ----eval=FALSE---------------------------------------------------------------
#  print(easyAnalysis)

## ----eval=FALSE---------------------------------------------------------------
#  # HRVIndices
#  head(easyAnalysis$HRVIndices)

## ----eval=FALSE---------------------------------------------------------------
#  # Statistical analysis
#  head(easyAnalysis$stats)

## ----results=FALSE, eval=FALSE------------------------------------------------
#  easyAnalysisWavelet <- RHRVEasy(
#    folders = c(NSR_DB, CHF_DB),
#    typeAnalysis = 'wavelet',
#    n_jobs = -1
#  )

## ----eval=FALSE---------------------------------------------------------------
#  easyAnalysisFDR <- RHRVEasyStats(easyAnalysis, correctionMethod =  'fdr')
#  pValues <- merge(
#    easyAnalysis$stats,
#    easyAnalysisFDR$stats,
#    by = setdiff(names(easyAnalysis$stats), "adj.p.value"),
#    suffixes = c(".bonf", ".fdr")
#  )
#  #Let us compare the p-values obtained with different correction methods
#  print(
#    head(
#      pValues[, c("HRVIndex", "p.value", "adj.p.value.bonf", "adj.p.value.fdr")]
#    )
#  )

## ----eval=FALSE---------------------------------------------------------------
#  easyAnalysis <- RHRVEasy(folders = c(NSR_DB, CHF_DB),
#                           saveHRVIndicesInPath = spreadsheetPath)

## ----eval=FALSE---------------------------------------------------------------
#  SaveHRVIndices(easyAnalysis, saveHRVIndicesInPath = spreadsheetPath)

## ----eval=FALSE---------------------------------------------------------------
#  #Comparison of the three databases
#  easyAnalysis3 <- RHRVEasy(
#    folders = c(NSR_DB, CHF_DB, HEALTHY_DB),
#    nJobs = -1
#  )
#  print(easyAnalysis3)

## ----eval=FALSE---------------------------------------------------------------
#  print(head(easyAnalysis3$stats))

## ----eval=FALSE---------------------------------------------------------------
#  # Let's print the post-hoc comparisons for "SDNN"
#  print(head(easyAnalysis3$stats$pairwise[[1]]))

## ----results=FALSE, eval=FALSE------------------------------------------------
#  easyAnalysisOverwritten <- RHRVEasy(folders = c(NSR_DB, CHF_DB),
#                                      freqhr = 2,
#                                      ULFmin = 0, ULFmax = 0.02,
#                                      VLFmin = 0.02,  VLFmax = 0.07,
#                                      LFmin = 0.07, LFmax = 0.20,
#                                      HFmin = 0.20, HFmax = 0.5)

## ----eval=FALSE---------------------------------------------------------------
#  fullAnalysis <- RHRVEasy(
#    folders = c(NSR_DB, CHF_DB, HEALTHY_DB),
#    nJobs = -1,
#    nonLinear =  TRUE,
#    doRQA = TRUE,
#    saveHRVIndicesInPath = spreadsheetPath
#  )

