# Private function for RHRV use only
# Modification of the graphics::filled.contour function that allows the 
# user to specify with the additional show.legend parameter if a legend
# should be added or not
rhrv.filled.contour <-
  function (x = seq(0, 1, length.out = nrow(z)), 
            y = seq(0, 1, length.out = ncol(z)), 
            z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, 
            color.palette = topo.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes, show.legend = TRUE,
            episodesColorPalette = NULL, markEpisodes = TRUE,
            ...) {
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  
  # functions for adding the legend
  if (show.legend){
    mar.orig <- (par(c("mar", "las", "mfrow")))$mar
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
                yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)  
    if (missing(key.axes)) {
      if (axes) 
        axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
      key.title
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
  }
    
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
  
}
