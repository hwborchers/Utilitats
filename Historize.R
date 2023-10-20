##   Historize functions

### Definition
#
#  Add history storage to a function
#  Histfun() will return the storage
#

Historize <- function (fun, len = 0, ...) {
    stopifnot(is.function(fun))
    if (len != floor(len) || len < 0) 
        stop("Argument 'len' must be a non-negative integer.")
    local({
        H1 <- H2 <- numeric()
        myFun <- function(x, ...) {
            if (missing(x)) {           # return storage
                if (len == 0) {
                    m1 <- 0; m2 <- length(H2)
                    return(list(input=H1, values=H2, nvars = 1, ncalls = m2))
                } else {
                    m1 <- length(H1); m2 <- length(H2)
                    if (m2 == 0) {
                        return(list(input = NULL, values = NULL,
                                    nvars = 0, ncalls = 0))
                    } else if (m1 %% m2 != 0) {
                        stop("Input length not consistent with no. of calls.")
                    } else {
                        n <- m1/m2
                    }
                    H1m <- matrix(H1, ncol = n, byrow = TRUE)
                    return(list(input=H1m, values=H2, nvars = n, ncalls = m2))
                }
            }
            else if (is.null(x)) {      # clear storage
                H1 <<- H2 <<- numeric()
                return(invisible(NULL))
            }
            else {
                y <- fun(x, ...)
                if (len == 0) {
                    H2 <<- c(H2, y)     # function values
                }
                else {
                    H1 <<- c(H1, x)     # input values
                    H2 <<- c(H2, y)     # function values
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
Fn0 <- Historize(adagio::fnRastrigin, len = 0)
Fn1 <- Historize(adagio::fnRastrigin, len = 1)

#-- Store values only
system.time(
    sol <- DEoptimR::JDEoptim(lb, ub, Fn0)  # 36 sec, 47000 evals
)  # 4.8 sec
fvalues <- Fn0()$values
plot(fvalues, type = 'p', col = "navy", pch = '.', cex = 1.5)

#-- Store input and values
system.time(
    sol <- DEoptimR::JDEoptim(lb, ub, Fn1)  # 30.5 sec
)
fvalues <- Fn1()$values
points(fvalues, type = 'p', col = "darkred", pch = '.', cex = 1.5)

