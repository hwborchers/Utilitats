### ---- U t i l i t i e s ---------------------------------------------
###
### File:    utils.R
### Author:  Hans W. Borchers
### Version  0.2-0
### Date:    2023-02-01
###
### --------------------------------------------------------------------


# call 'ans' [not 'ans()' !] for last function value
makeActiveBinding("ans", function() .Last.value, .GlobalEnv)  # ans

# Define your own binary operators
'%nin%' <- function(x, table) match(x, table, nomatch = 0) == 0
'%//%'  <- function(p, q) gmp::as.bigq(p, q)



## general helpful minis -----------------------------------------------

first <- function(a) c(a)[1]
last <- function(a) c(a)[length(a)]

clear <- function() rm(list=ls(all.names=TRUE))

disp <- function(...) cat(..., "\n")
display <- function (...) {
    evaluatedArgs <- list(...)
    n <- length(evaluatedArgs)
    argTags <- names(evaluatedArgs)
    deparsedArgs <- lapply(substitute(placeholderFunction(...))[-1], 
                           function(expr) {
                               d <- deparse(expr)
                               paste(d, collapse = "\n        ")
                           }
    )
    if (is.null(argTags)) argTags <- rep("", n)
    namedArgs <- ifelse(argTags != "", argTags, deparsedArgs)
    cat(paste(namedArgs, evaluatedArgs, sep=" = "), sep = "\n")
}

# Count number of elements, similar to 'table'
count <- function(x, sorted = TRUE) {
    # two times faster than `table`, returns integers
    x = c(x); n = length(x)
    if (n == 0) {
        stop("Argument 'x' must not be empty.")
    } else if (n == 1) {
        return(list(v = x[1], e = c(1)))
    }
    if (sorted && is.unsorted(x)) x = sort(x)
    
    v = c(); e = c()
    x0 <- x[1]; e0 = 1
    for (i in 2:n) {
        if (x[i] == x0) {
            e0 = e0 + 1
        } else {
            v = c(v, x0); e = c(e, e0)
            x0 = x[i]; e0 = 1
        }
    }
    v = c(v, x0); e = c(e, e0)
    return(list(v = v, e = e))
}

# Fence values into interval [a, b]
clipit <- function(x, a, b) pmax( a, pmin(x, b))
inside <- function(x, a, b) if (a <= x && x <= b) TRUE else FALSE

# Insert constant values into vector
insert <- function(x, a) {
    n <- 2*length(x)-1; xa <- rep(a, n)
    xa[seq(1, n, by=2)] <- x; xa }

# Find all indices i such that v[i, ..., i+m-1] == p
occurs <- function(p, v){
    m <- length(p); n <- length(v)
    inds <- seq.int(length = n-m+1)
    for (i in seq.int(length = m))
        inds <- inds[p[i] == v[inds + i - 1]]
    inds
}

# multiple (list) assignments or multiple returns
# list2env(L, envir = environment())
deal <- function(L, Env = parent.env(environment())) {  # or: .GlobalEnv)
    for (name in names(L)) assign(name, L[[name]], envir = Env)
}

## Define more help entries resp. comments -----------------------------

# Redefines the '?' function
`?` <- function(...) {
    if (!is.null(doc <- comment(get(match.call()[[2]])))) {  # WRONG?!
        cat(doc, "\n")
    } else {
        help(...)
    }
}

# 'comment' (queries or) sets a comment attribute (in Base R)
comment(clear) <- "Clears all variables from the workspace - watch out!"


# EoF ------------------------------------------------------------------
