# Linear and multiplicative random number generators (RNGs)
# http://www.aaronschlegel.com/series/random-number-generation/#

library(pracma) #: mod()

### Multiplicative congruential generators, also known as Lehmer random number
### generators, is a type of linear congruential generator for generating 
### pseudorandom numbers in (0,1). The multiplicative congruential generator, 
### often abbreviated as MLCG or MCG, is defined as a recurrence relation 
### similar to the LCG with c=0.
lehmer.rng <- function(n=10, d = as.numeric(Sys.time())) {
  rng <- vector(length = n)
  m <- 2147483647
  a <- 48271
  q <- 44488        # q = m/a
  r <- 3399         # r = m mod a
  
  for (i in 1:n) {
    h <- d / q
    l <- d %% q
    t <- a * l - r * h
    if (t < 0) {
      d <- t + m
    } else if (t > 0) {
      d <- t
    } else {
      d <- m - 1
    }
    rng[i] <- d / m
  }
  return(rng)
}

### A Linear congruential generator (LCG) is a class of pseudorandom number 
### generator (PRNG) algorithms used for generating sequences of random-like 
### numbers, defined by a recurrence relation: x_{i+1} = (a x_i + c) mod m.
linear.rng <- function(n=10) {
  rng <- vector(length = n)
  m <- 2 ** 32
  a <- 1103515245  # NR: 1664525
  c <- 12345       # NR: 1013904223
  # Set the seed using the current system time in microseconds
  d <- as.numeric(Sys.time()) * 1000
  for (i in 1:n) {
    d <- (a * d + c) %% m
    rng[i] <- d / m
  }
  return(rng)
}


### Combined linear congruential generators are a type of PRNG that combine two 
### or more LCGs. The combination of two or more LCGs into one random number
### generator can result in a marked increase in the period length of the 
### generator which makes them suited for simulating more complex systems.
lincom.rng <- function(n=10) {
  rng <- vector(length = n)
  a1 <- 40014
  m1 <- 2147483563
  a2 <- 40692
  m2 <- 2147483399

  # Seed the two MCGs
  y1 <- lehmer.rng()    # random integer in [1, m1-1]
  y2 <- lehmer.rng()    # random integer in [1, m2-1]

  for (i in 1:n) {
    y1 <- a1 * y1 %% m1
    y2 <- a2 * y2 %% m2
    x <- (y1 - y2) %% (m1 - 1)
    if (x > 0) {
      rng[i] <- x / m1
    }
    else if (x < 0) {
      rng[i] <- (x / m1) + 1
    }
    else if (x == 0) {
      rng[i] <- (m1 - 1) / m1
    }
  }
  return(rng)
}


### This shows how to use 'external' storage through the `local()` construct.
myRand <- function(seed = as.numeric(Sys.time())) {
    local({
        R <- vector(mode = "numeric", length = 2000)
        R[1:100] <- lehmer.rng(100, seed)
        for (k in 101:2000)
            R[k] <- mod(R[k-37] + R[k-100], 1.0)
        k <- 2000; i <- 2000 - 37; j <- 2000 - 101
        frand <- function() {
            k <<- (k %% 2000) + 1
            i <<- (i %% 2000) + 1
            j <<- (j %% 2000) + 1
            z <- mod(R[i] + R[j], 1.0)
            R[k] <<- z
            return(z)
        }
        return(frand)
    })
}

### Example: Generate 1000 random numbers, one by one.
zuza <- myRand(5689)
N = 1000
x = numeric(N)
for (i in 1:N) 
    x[i] = zuza()
hist(x)


