## R graphics ----------------------------------------------------------

opar <- par(bty = 'L', mar = c(2,2,2,1),
            font.main = 2)

# plot(..., asp = 1)

# ... assume that plot() has been called
circles <- function(x, y, r, n = 100, ...) {
    # C = (x, y, r)
    z  <- seq(0, 2*pi, length.out = n)
    xs <- x + r * cos(z)
    ys <- y + r * sin(z)

    # border color: border = NULL; fill color: col = NA
    polygon(xs, ys, ...)
    invisible(NULL)
}

plotfn <- function(fn, a, b, n = 100, col = 4, lwd = 2, lty = 1,
                   grid = TRUE, main = "Function Plot", ...) {
    xs <- seq(a, b, length.out = n)
    ys <- numeric(n)
    for (i in 1:n) ys[i] <- fn(xs[i])
    plot(xs, ys, type = 'l', col = col, lwd = lwd,
         xlab = 'x', ylab = 'y', main = main, ...)
    if (grid) grid()
    invisible(NULL)
}

par(opar)

# save plots in PDF, PNG, or SVG format
# plot(), grid(), and then "dev.off()"
# pdfcrop filename.pdf filename.pdf

savepdf <- function(fname, width=12, height=9)  # [cm]
{
  fname <- paste(fname, ".pdf", sep = "")
  pdf(fname, width = width/2.54, height = height/2.54, pointsize=10)
  par(mgp = c(2.2, 0.45, 0),        # axis label, tick mark labels, tick lines
      tcl = -0.4,                   # length of axis ticks
      mar = c(3.3, 3.6, 2.1, 2.1),  # margins below, left, above, right
      font.main = 1)                # normal font, not bold
}

savepng <- function(fname, width=640, height=360, ...)  # [px]
{
    fname <- paste(fname, ".png", sep = "")
    png(fname, width = width, height = height, pointsize = 12, bg = "white",
        type = "cairo-png", ...)
}


## ggplot --------------------------------------------------------------

## EoF -----------------------------------------------------------------
