##   Historize functions

### Definition
#
#  Add history storage to a function
#  histfun() will return the storage
#

historize <- function(fun, len = 0) {
    if (!is.function(fun))
        stop("'fun' must be a (numeric) function.")
    if (len != floor(len) || len < 0)
        stop("'len' must be a non-negative integer.")
    local({
        H <- numeric()
        myFun <- function(x) {
            if(missing(x)) {
                return(H)
            } else if (is.null(x)) {
                H <<- numeric()
                return(invisible(NULL))
            } else {
                y <- fun(x)     # storing parameters is slow
                if (len == 0) {
                    H <<- c(H, y)
                } else
                    H <<- c(H, x, y)    # or: rbind(H, c(x, y))
                return(y)
            }
        }
        return(myFun)
    })
}

Historize <- function(fun, len = 0, ...) {
    if (!is.function(fun))
        stop("'fun' must be a (numeric) function.")
    if (len != floor(len) || len < 0)
        stop("'len' must be a non-negative integer.")
    local({
        H <- numeric()
        myFun <- function(x, ...) {
            if(missing(x)) {
                return(H)
            } else if (is.null(x)) {
                H <<- numeric()
                return(invisible(NULL))
            } else {
                y <- fun(x, ...)
                if (len == 0) {
                    H <<- c(H, y)
                } else {
                    if (length(x) != len)
                        stop("Incorrect parameter length.")
                    H <<- rbind(H, c(x, y))
                }
                return(y)
            }
        }
        return(myFun)
    })
}


#-- Example ------------------------------------------------------------
#
#   We will use the Rastrigin function with DEoptimR
#
dm <- 10
lb <- rep(-5.2, 10); ub <- -lb
Fn1 <- historize(adagio::fnRastrigin, len = dm)
Fn2 <- Historize(adagio::fnRastrigin, len = dm)

#-- Fn1(NULL)
system.time(
    sol <- DEoptimR::JDEoptim(lb, ub, Fn1)  # 36 sec, 47000 evals
)  # 40 sec, 490000 evals
H <- matrix(Fn1(), ncol = dm+1, byrow = TRUE)
fvalues <- H[, dm+1]
plot(fvalues, type = 'p', col = "navy", pch = '.', cex = 1.5)

#-- Fn2(NULL)
system.time(
    sol <- DEoptimR::JDEoptim(lb, ub, Fn2)  # 32 sec, 46000 evals
)
H <- Fn2()
fvalues <- H[, dm+1]
plot(fvalues, type = 'p', col = "darkred", pch = '.', cex = 1.5)

#-- Fn(NULL)
Fn <- Historize(adagio::fnRastrigin)
system.time(
    sol <- DEoptimR::JDEoptim(lb, ub, Fn)  # 4.8 sec. 46000 evals
)
fvalues <- Fn()
plot(fvalues, type = 'p', col = "navy", pch = '.', cex = 1.5)

