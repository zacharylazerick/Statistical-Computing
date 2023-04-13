## Homework 4

## 1 a

NormSim <- function(n, a) {
  counts <- c();
  for (i in 1:n) {
    X <- 0; counter <- 0
    while (X < a) {
      X <- rnorm(1, 0, 1)
      counter <- counter + 1
    }
    counts <- append(counts, counter)
  }
  return(counts)
}

a.1 <- NormSim(1000, 1)
round(mean(a.1), digits = 0)

a.2 <- NormSim(1000, 2)
round(mean(a.2), digits = 0)

a.3 <- NormSim(1000, 3)
round(mean(a.3), digits = 0)

a.4 <- NormSim(1000, 4)
round(mean(a.4), digits = 0)


## 1 b

NormSim.V2 <- function(n, a) {
  counts <- c();
  for (i in 1:n) {
    X <- 0; U <- 1; counter <- 0; 
    while (U > exp(-(X - a)^2/2)) {
      Y <- rexp(1, a); U <- runif(1)
      X <- Y + a; counter <- counter + 1
    }
    counts <- append(counts, counter)
  }
  return(counts)
}

a.1 <- NormSim.V2(1000, 1)
mean(a.1)

NormSim.V3 <- function(n, a) {
  counts <- c();
  for (i in 1:n) {
    X <- 0; U <- 1; counter <- 0; 
    while (U > exp(-(X - a)^2/2)) {
      Y <- rexp(1, a); U <- runif(1)
      X <- Y + a; counter <- counter + 1
      if (U <= exp(-(X - a)^2/2)) {
        if (runif(1) > .5) {
          break
        }
        else {
          U <- 1
        }
      }
    }
    counts <- append(counts, counter)
  }
  return(counts)
}

a.1 <- NormSim.V3(1000, 1)
mean(a.1)

## 2 a

NormSim.V3 <- function(n) {
  X <- c(); counter <- 0
  while (length(X) < n) {
    Y <- rexp(1); U <- runif(1)
    f.Y <- exp((-(Y^2)/2) + Y - (1/2))
    if (U <= f.Y) {
      X <- append(X, Y)
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  for (j in 1:length(X)) {
    U <- runif(1)
    if (U < .5) {
      X[j] <- X[j] * -1
    }
  }
  mylist <- list('X' = X, 'Proposals' = counter)
  return(mylist)
}

## 2 b

## Store Simulated Z-scores
HalfNorm <- NormSim.V3(1000)

## Convert Z-scores to N(5, 10)
HalfNorm <- HalfNorm * sqrt(10) + 5

mean(HalfNorm)
var(HalfNorm)

## 2 c

Norm <- NormSim.V3(25000)

counter <- 0
for (i in 1:length(Norm$X)) {
  if (Norm$X[i] > -1.96 & Norm$X[i] < 1.96) {
    counter <- counter + 1
  }
}

counter/length(Norm$X)

## 2 d

CauchySim <- function(n) {
  X <- c(); counter <- 0; A <- sqrt(2 * pi)*(0.6065306597)
  while (length(X) < n) {
    ## Generate Cauchy Variables
    U <- runif(1); Y <- tan(pi*(U - (1/2)))
    f.Y <- (1/sqrt(2*pi))*exp(-(Y^2)/2); g.Y <- 1/(pi*(1 + Y^2))
    if (U <= (f.Y/(A*g.Y))) {
      X <- append(X, Y)
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list('X' = X, 'Proposals' = counter)
  return(mylist)
}

Cauchy <- CauchySim(25000)

counter <- 0
for (i in 1:length(Cauchy$X)) {
  if (Cauchy$X[i] > -1.96 & Cauchy$X[i] < 1.96) {
    counter <- counter + 1
  }
}

counter/length(Cauchy$X)

## 2 e

Norm$Proposals; Cauchy$Proposals

## 3 a

SemiRejection <- function(n) {
  X <- c(); U <- c(); counter <- 0
  while (length(X) < n) {
    Unif <- runif(1); Y <- runif(1, -1, 1)
    if (Unif <= (1 - Y^2)^(1/2)) {
      X <- append(X, Y); U <- append(U, Unif)
      counter <- counter + 1
    }
    else {
      coutner <- counter + 1
    }
  }
  mylist <- list('X' = X, 'U' = U, 'Proposals' = counter)
  return(mylist)
}

## Create Sample
SemiCircle <- SemiRejection(1000)

## Scatterplot
plot(SemiCircle$X, SemiCircle$U)

## How many proposals needed?
SemiCircle$Proposals

## 4 a

BetaSim <- function(n, alpha, beta) {
  X <- c(); counter <- 0; Y <- 0; A = (16/9)
  while (length(X) < n) {
    Y <- runif(1); U <- runif(1)
    f.Y <- 12*Y^(alpha - 1)*(1-Y)^(beta - 1)
    if (U <= (f.Y/A)) {
      X <- append(X, Y); counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist = list('X' = X, 'Proposals' = counter)
  return(mylist)
}

Beta <- BetaSim(1000, 3, 2)
    
hist(Beta$X, freq = FALSE)
x <- seq(0, 1, .01); DBETA <- dbeta(x, 3, 2)
lines(x, DBETA, col = "red", lwd = 2)

## 4 d
system.time(BetaSim(1000, 3, 2)); system.time(dbeta(1000, 3, 2))

## 5 

ExpSim <- function(n) {
  X <- c();
  while (length(X) < n) {
    U <- runif(1)
    Y <- log((1 - U*(1 - exp(.05))))
    X <- append(X, Y)
  }
  return(X)
}

Exp <- ExpSim(1000)

mean(Exp)

U <- runif(1)
Y <- log((1 - U*(1 - exp(.05))))
X <- append(X, Y)

exp(.05)

## 6
Insurance <- function(n) {
  Total_Claims <- c()
  for (i in 1:n) {
    Claims <- 0; 
    U <- runif(1000); E <- rexp(1000, 1/800)
    for (j in 1:1000) {
      if (U[j] <= 0.05) {
        Claims <- Claims + E[j]
      }
    }
    Total_Claims <- append(Total_Claims, Claims)
  }
  return(Total_Claims)
}

Claims <- Insurance(1000)

count <- 0
for (i in 1:length(Claims)) {
  if (Claims[i] > 50000) {
    count <- count + 1
  }
}
count / 1000

RejectionSampler <- function(n) {
  X <- c(); counter <- 0
  while (length(X) < n) {
    Y <- runif(1); U <- runif(1)
    if ((U <= (1/4) + 2*Y^3 + (5/4) * Y^4)/(1/4)) {
      counter <- counter + 1
      X <- append(X, U)
    }
    counter <- counter + 1
  }
  mylist <- list('X'= X, 'counter' = counter)
  return(mylist)
}

X <- RejectionSampler(1000)

## Histogram
hist(X$X)
 
## Number of Samples Generated for 1000 Acceptanced Samples
X$counter

RejectionSampler8 <- function(n) {
  X <- c(); counter <- 0
  while (length(X) < n ) {
    Y <- (1/2) * rexp(1, mean = (1/2)); U <- rexp(1)
    f.Y <- (1 + Y)*exp(Y)
    if (U <- f.Y) {
      X <- append(X, Y); counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list('X' = X, 'Proposals' = counter)
  return(mylist)
}

Results8 <- RejectionSampler8(1000)

hist(Results8$X, freq = FALSE)
x <- seq(0, 10, .5); equation <- (1/2) * (1 + x) * exp(x)
lines(x, equation, col = 'red', lwd = 2)

mean(Results8$X); var(Results8$X)
