# filter non-scaling regions ----------------------------------------------

nltsFilter <- function(x, ...) {
  UseMethod("nltsFilter", x)
}

# @importFrom nonlinearTseries corrDim
#' @export
nltsFilter.corrDim <- function(x, threshold = 0.95, ...) {
  rgs <- list(...)
  if (threshold < 0 || threshold > 1) {
    stop("threshold for corrDim objects should be in [0,1]")
  }
  indx <- which(x$corr.matrix > threshold, arr.ind = TRUE)
  # pick the columns and eliminate those values in the corrMatrix and the radius
  colValues <- unique(indx[,2])
  if (length(colValues) > 1 ) {
    x$corr.matrix <- x$corr.matrix[,-colValues,drop = FALSE]
    x$radius <- x$radius[-colValues]
  }
  x
}

# @importFrom nonlinearTseries maxLyapunov
# @importFrom stats ksmooth median
#' @export
nltsFilter.maxLyapunov <- function(x, kernel = "normal", bandwidth = 2, ...) {
  smoothedS <- apply(x$s.function, 1, function(sf, time, kernel, bandwidth) {
    ksmooth(time, sf, kernel, bandwidth)
  }, time = x$time, kernel = kernel, bandwidth = bandwidth)
  smoothedS <- t(sapply(smoothedS, FUN = function(x) x$y))
  # plot(x$time, smoothedS[1,],type="l")
  # for (i in 2:nrow(smoothedS)) {
  #   lines(x$time, smoothedS[2,], col =2)
  # }
  localMax <- apply(smoothedS, 1, function(x) {
    indx <- which(diff(sign(diff(x))) == -2) + 1
    if (length(indx) == 0) {
      return(length(x))
    } else {
      return(min(indx))
    }
  })
  indx <- floor(median(localMax))
  x$s.function <- x$s.function[,1:indx]
  x$time <- x$time[1:indx]
  x
}

# Estimate local slopes using kernels -----------------------------------------
# @importFrom graphics lines
estimateLocalSlopes <- function(y, x, bandwidth, kernel, doPlot = FALSE) {

  checkSegmentArguments(y, x)
  # ksmooth requires x to be in increasing order
  indx <- order(x)
  x <- x[indx]
  y <- y[,indx]
  # smoothDiff is a list of the same length as the number of the embedding dimensions
  # stored in the matrix y. Each slot corresponds to a different embedding dimension,
  # and it contains the 'x' variable and 'y' (which is an
  # estimate of the derivative of each row of the matrix y with respect to x).
  smoothDiff <- apply(y, MARGIN = 1,
                     FUN = easyDifferentiate,
                     x = x,
                     bandwidth = bandwidth, kernel = kernel)
  # 'x' is the same for all embedding dims: just pick the first
  smoothDiffRadius <- smoothDiff[[1]]$x
  # Reorder smoothDiff as a matrix in which each row is a different embedding dim
  smoothDiff <- t(sapply(smoothDiff, function(x) x$y))
  naCols <- unique(which(is.na(smoothDiff), arr.ind = TRUE)[,2])
  if (length(naCols > 1)) {
    warning("NAs in the estimation of the local slopes. It may be due to a low
            bandwidth (increase it to avoid problems)")
    smoothDiffRadius <- smoothDiffRadius[-naCols]
    smoothDiff <- smoothDiff[,-naCols]
  }
  if (doPlot) {
    try({
      # plot to check the local slopes
      plot(smoothDiffRadius, smoothDiff[1,], ylim = range(smoothDiff),
           type = "l", main = "Smoothed local slopes")
      for (i in 2:nrow(smoothDiff)) {
        lines(smoothDiffRadius, smoothDiff[i,],col = i)
      }
    })
  }
  list(y = smoothDiff, x = smoothDiffRadius)
}


# estimate derivative using a kernel to smooth the result and avoid peaks
# @importFrom stats ksmooth
easyDifferentiate <- function(y, x, kernel = c("normal","box"), bandwidth = 0.5){
  kernel <- match.arg(kernel)
  len <- length(y)
  #  symmetric difference quotient: (y(x + h) - y(x - h) )/ 2h
  if (len >= 3) {
    ksmooth(x[-c(1, len)], (y[3:len] - y[1:(len - 2)]) / (x[3:len] - x[1:(len - 2)]),
            kernel, bandwidth)
  } else{
    # if not possible... use (y(x+h)-y(x))/h
    ksmooth(x[-len], diff(y) / diff(x),
            kernel, bandwidth)
  }
}

# Run segmented but filtering the warning "No breakpoint estimated"
# @importFrom segmented segmented
# @importFrom stats lm
doSegmentation <- function(data, initialValues) {
  handler <- function(w) {
    if (any(grepl("No breakpoint estimated", w, "\n"))) {
      # "muffleWarning" implements a simple recovery strategy: “Suppress the warning”
      invokeRestart("muffleWarning")
    }
  }
  withCallingHandlers({
    fit <- lm(y ~ x, data = data)
    segmented(fit, psi = initialValues)
  },
  warning = handler
  )
}

#  estimate the scaling regions  --------------------------------------

# @importFrom graphics abline points
# @importFrom stats lm coef
segmentAndSelectBySlope <- function(y, x, initialValues, criterion = c("max", "min"),
                                   doPlot = FALSE) {
  criterion <- match.arg(criterion)
  data <- data.frame(y = y, x = x)

  segmentedFit <- doSegmentation(data, initialValues)
  # segmentedFit <- segmented(fit, seg.Z = ~x, psi = initialValues)

  if (is.null(segmentedFit$id.group)) {
    # Single slope found. The whole x is the scaling region
    scalingRegion <- x
    if (doPlot) {
      plot(y ~ x, data)
      abline(lm(y ~ x, data), col = 2)
    }
  } else {
    absSlopes <- lapply(split(data, segmentedFit$id.group),
                       function(data){
                         # calculate the slope of the corresponding group
                         abs(coef(lm(y ~ x, data = data))[[2]])
                       })
    # use for debugging
    if (doPlot) {
      plot(y ~ x, data)
      plot(segmentedFit, add = TRUE, col = 2)
      breakPoints = sapply(segmentedFit$psi[,2], function(point) which.min(abs(data$x - point)))
      points(data$x[breakPoints], segmentedFit$fitted.values[breakPoints], col = 2, bg = 2, pch = 22)
    }
    # identify the scaling_region as the region with the minimum/maximum slope (in abs value)
    criterionFun <- switch(criterion,
                          "max" = which.max,
                          "min" = which.min)
    scalingRegionGroup <- names(absSlopes)[[criterionFun(absSlopes)]]
    scalingRegion <- x[scalingRegionGroup == segmentedFit$id.group]
  }

  range(scalingRegion)
}

# @importFrom stats quantile
estimateAllScalingRegions <- function(y, x, numberOfLinearRegions, initialValues,
                               criterion = c("max", "min")) {
  criterion <- match.arg(criterion)
  checkSegmentArguments(y, x)
  if (is.null(initialValues)) {
    # select initial values for the segmentation using quantiles (not optimal).
    # TODO: It may be a good idea to select the initial points depending on the curvature.
    probs <- seq(0, 1, len = numberOfLinearRegions + 1)[-c(1,numberOfLinearRegions + 1)]
    initialValues <- quantile(x,  probs = probs)
  }
  if ((length(initialValues) + 1) != numberOfLinearRegions) {
    stop("numberOfLinearRegions should be equal to length(initialValues") - 1
  }

  scalingRegion <- t(apply(y, MARGIN = 1, FUN = segmentAndSelectBySlope,
                          x = x, initialValues = initialValues,
                          criterion = criterion,
                          doPlot = FALSE))
  colnames(scalingRegion) <- c("scalingRegionStart","scalingRegionEnd")
  scalingRegion
}

# estimateScalingRegion -------------------------------------------------

# Estimates the scaling region of a nonlinearTseries object
estimateScalingRegion <- function(x, numberOfLinearRegions,
                                  initialValues, doPlot, ...) {
  UseMethod("estimateScalingRegion", x)
}

# Estimate the scaling region of a corrDim object.
# @details
# Estimates the scaling region of a corrDim object by:
# 1.- calculating smooth local slopes (using the 'ksmooth' with the  kernel
# specified by 'kernel' and the bandwidth 'bandwidth'. If they are not specified,
# a gaussian kernel and a proper bandwidth will be used.)
# 2.- segmentating in straight lines the smooth local slopes. The number of
# linear slopes used to fit the data is specified by 'numberOfLinearRegions'.
# The initial breakPoints that the method requires can be specified by 'initialValues'.
# If they are not specified, the method selects them automatically.
# It must be noted that 'length(initialValues) + 1 = numberOfLinearRegions'.
# @importFrom nonlinearTseries plotLocalScalingExp
# @importFrom graphics abline
# @importFrom stats sd
#' @export
estimateScalingRegion.corrDim <- function(x, numberOfLinearRegions = 4,
                                           initialValues = NULL, doPlot = FALSE,
                                           bandwidth = NULL,
                                           kernel = c("normal", "box"), ...) {
  TRUST_THRESHOLD <- 0.5
  # rename for convenience
  cd <- x
  kernel <- match.arg(kernel)
  if (is.null(bandwidth)) {
    # since the derivatives are calculated in log scale the bandwidth should be
    # specified in log scale. Since the distance in log space is
    meanLogDistance <-  abs(mean(diff(log10(cd$radius))))
    # we may select a multiple  of that value m * meanLogDistance: m being the
    # average number of points that will be used to compute the local scales.
    # By default, we use 10 points.
    bandwidth <- 10 * meanLogDistance
  }
  # find local slopes per embedding dimension
  localSlopes <-
    estimateLocalSlopes(y = log10(cd$corr.matrix),
                        x = (cd$corr.order - 1) * log10(cd$radius),
                        bandwidth = bandwidth, kernel = kernel,
                        doPlot = FALSE)
  scalingRegionMatrix <- estimateAllScalingRegions(localSlopes$y, localSlopes$x,
                                                   numberOfLinearRegions, initialValues,
                                                   criterion = "min")
  # scalingRegionMatrix has an estimate per embedding dimension of the scalingRegion region in
  # terms of the '(cd$corr.order - 1) * log10(cd$radius)' variable. Solve for
  # 'radius'
  scalingRegionRadius <- apply(scalingRegionMatrix, MARGIN = 1,
                              function(x) { 10 ^ (x / (cd$corr.order - 1)) })
  # calculate the mean of the different estimations from the different embeddings
  meanScalingRegionRadius <- rowMeans(scalingRegionRadius)
  meanScalingRegionTransformedRadius <- colMeans(scalingRegionMatrix)

  if (doPlot) {
    try({
      # don't add legend since it avoid the correct placement of the scaling
      # region
      plotLocalScalingExp(cd, add.legend = FALSE,
                          main = 'estimate of the scaling region')
      abline(v = meanScalingRegionRadius, col = "black", lwd = 3, lty = 2)
    })
  }

  scalingRegionIndicator <- (
    (localSlopes$x >= meanScalingRegionTransformedRadius[1]) &
    (localSlopes$x <= meanScalingRegionTransformedRadius[2])
  )

  reliableScalingRegion <-  (
    sd(localSlopes$y[, scalingRegionIndicator]) > TRUST_THRESHOLD
  )

  list("scalingRegion" = meanScalingRegionRadius, "reliable" = reliableScalingRegion)
}

# estimates the scaling region of a maxLyapunov object by:
# 1.- segmentating in linear slopes the divergence function S(t). The number of
# linear slopes used to fit the data is specified by 'numberOfLinearRegions' (
# by default 2: one for the scaling region, and other for the saturated region).
# The initial breakPoints that the method requires can be specified by 'initialValues'.
# If they are not specified, the method selects them automatically.
# It must be noted that 'length(initialValues) + 1 = numberOfLinearRegion'.
#' @export
estimateScalingRegion.maxLyapunov <- function(x, numberOfLinearRegions = 2,
                                               initialValues = NULL, doPlot = FALSE, ...) {
  # find scaling regions per embedding dimension
  scalingRegionMatrix <- estimateAllScalingRegions(x$s.function[, -1], x$time[-1],
                                                   numberOfLinearRegions, initialValues,
                                                   criterion = "max")
  # calculate the mean of the different estimations from the different embeddings
  meanScalingRegionRadius <- colMeans(scalingRegionMatrix)

  if (doPlot) {
    try({
      plot(x, type = "l", add.legend = FALSE, main = "Estimate scaling region")
      abline(v = meanScalingRegionRadius, col = "black", lwd = 3, lty = 2)
    })
  }
  meanScalingRegionRadius
}


checkSegmentArguments <- function(y, x) {
  if (!inherits(y, "matrix")) {
    stop("'y' should  be a matrix")
  }
  if (ncol(y) != length(x)) {
    stop("ncol(y) != length(x)")
  }
}
