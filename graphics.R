opar <- par()

# ... assume that plot() has been called
circle <- function(x, y, r, n = 100, ...) {
    # C = (x, y, r)
    z = seq(0, 2*pi, length.out = n)
    xs = x + r * cos(z)
    ys = y + r * sin(z)

    # border color: border = NULL; fill color: col = NA
    polygon(xs, ys, ...)
    invisible(NULL)
}

par(opar)

### EoF ----------------------------------------------------------
