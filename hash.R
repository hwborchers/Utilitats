# ____________________________________________________ H a s h i n g ___
## Define functions on Hash Tuples (Python-alike))

def.h <- function() new.env(hash=TRUE, parent=emptyenv())
len.h <- function(dict) length(ls(envir=dict))
set.h <- function(key, val, dict) assign(as.character(key), val, envir=dict)
get.h <- function(key, dict, default=NULL) {
    key <- as.character(key)
    if (exists(key, envir=dict)) { get(key, dict)
    } else { default }
}
del.h <- function(key, dict) {
    key <- as.character(key)
    if (exists(key, envir=dict)) {
        val <- get.h(key, dict)
        rm(list=c(key), envir=dict)
    } else {
        val <- NULL
    }
    invisible(val)
}
has_key <- function(key, dict) exists(as.character(key), envir=dict)
keys.h <- function(dict) ls(envir=dict)
items.h <- function(dict) as.list(dict)
values.h <- function(dict, mode='character') {
    l <- as.list(dict)
    n <- length(l)
    if (n==0) invisible(NULL)
    v <- vector('character', n)
    for (i in 1:n) v[i] <- l[[i]]
    if (mode=='numeric') v <- as.numeric(v)
    return(v)
}
clear.h <- function(dict) {
    rm(list=keys.h(dict), envir=dict)
}

# ----------------------------------------------------------------------
## How to define a simple 'table'-like counter
## counter <- def.h()
## count <- function(x, hash = counter) {
##      set.h(x, get.h(x, counter, default=0) + 1, counter)
## }

## Example: counting random LETTERS
## C = sample(LETTERS, 1000, replace = TRUE)
## for (ch in C) count(ch, counter)
## Cdf = data.frame(Letter = keys.h(counter),
##                  Number = as.numeric(values.h(counter)))
## head(Cdf)

## How to define a 'multiple'-counter
## defCounter <- function(n = 0) {
##     local({
##         count <- n
##         counter <- function(i = 1) {
##             if (missing(i)) return(count)
##             else count <<- count + i
##             invisible(count)
##         }
##         counter
##     })
## }

## Example
## count <- defCounter()
## for (i in 1:100) count(i)
## count()         #=> 5050
# ----------------------------------------------------------------------
