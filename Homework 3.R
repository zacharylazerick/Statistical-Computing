## Homework 3 

RandomPermutations <- function(n) {
  X <- sample(x = c(1:(n-1)), size = n - 1, replace = F); X <- append(X, n)
  U <- runif(1); temp <- X[floor(n*U + 1)];
  X[floor(n*U + 1)] <- X[n]; X[n] <- temp
  X
}

RandomPermutations(10)
RandomPermutations(10)
RandomPermutations(10)
RandomPermutations(10)
RandomPermutations(10)

total_hits <- rep(0, 1000)
for (i in 1:1000) {
  counter <- 0;  X <- RandomPermutations(100)
  for (j in 1:length(X)) {
    if (j == X[j]) {
      counter <- counter + 1
    }
  }
  total_hits[i] <- counter
}

mean(total_hits)
var(total_hits)

## Question 2

total_rolls <- rep(0, 10)
for (i in 1:10) {
  counter <- 0; Table_Roll_Sums <- rep(0, 11);
  while (0 %in% Table_Roll_Sums) {
    Dice <- sample(x = c(1, 2, 3, 4, 5, 6), size = 2, replace = T)
    Roll <- sum(Dice)
    Table_Roll_Sums[Roll - 1] <- Table_Roll_Sums[Roll - 1] + 1
    counter <- counter + 1
  }
  total_rolls[i] <- counter
}

## Mean
mean(total_rolls)

## Histogram of Results
hist(total_rolls)

## Buses 5 per hour --> \lambda = 5 --> \beta = 1/5
total_fans <- rep(0, 1000)
for (i in 1:1000) {
  t = 0; counter <- 0;
  while (t < 1) {
    U <- runif(1); X <- (-1/5)*log(1-U)
    t <- t + X; counter <- counter + 1
  }
  fans <- sample(x = 20:40, size = (counter - 1), replace = T)
  total_fans[i] <- sum(fans)
}

## Histogram
hist(total_fans)


GeometricGenerator <- function(n, p) {
  total_until_success <- rep(0, n)
  for (i in 1:n) {
    U <- runif(1); X <- ceiling(log(1-U)/log(1- p))
    total_until_success[i] <- X
  }
  return(table(total_until_success))
}

set.seed(0719)
GeometricGenerator(7500, .83)

BinomialGenerator <- function(m, n, p) {
  total_success <- rep(0, m)
  for (i in 1:m) {
    U <- runif(n); X <- rep(0, length(U))
    for (j in 1:length(U)) {
      if (U[j] < p) {
        X[j] = 1
      }
    }
    total_success[i] <- sum(X)
  }
  return(table(total_success))
}

set.seed(1923)
BinomialGenerator(7500, 200, .11)

PoissonGenerator <- function(n, l) {
  total_Poisson <- rep(0, n)
  for (i in 1:n) {
    t = 0; counter <- 0;
    while (t < 1) {
      U <- runif(1); X <- (-1/l)*log(1-U)
      t <- t + X; counter <- counter + 1
    }
    total_Poisson[i] <- counter - 1
  }
  return(table(total_Poisson))
}

set.seed(724)
PoissonGenerator(7500, 2.1)

GeometricGenerator <- function(p) {
    U <- runif(1); X <- ceiling(log(1-U)/log(1- p))
}

X <- GeometricGenerator(1/6); Y <- GeometricGenerator(1/6)
X == Y

GenerateX <- function(n) {
  X <- rep(0, n)
  for (i in 1:n) {
    U <- runif(1)
    X[i] <- -1/(U-1)
  }
  return(X)
}

X <- GenerateX(10000)

GenerateX <- function(n) {
  X <- rep(0, n)
  for (i in 1:n) {
    U <- runif(1)
    X[i] <- log(x = (exp(1) - 1)*(U + (1/(exp(1)-1))), base = exp(1))
  }
  return(X)
}

X <- GenerateX(10000)
min(X); max(X)

GenerateWeibull <- function(n, alpha, beta) {
  X <- rep(0, n)
  for (i in 1:n) {
    U <- runif(1)
    X[i] <- (log(1 - U)/-alpha)^(1/beta)
  }
  return(X)
}

results9 <- GenerateWeibull(1000, 1, 5)
min(results9); max(results9)

GenerateLaplace <- function(n) {
  X <- rep(0, n)
  for (i in 1:n) {
    U <- runif(1)
    if (U < (1/2)) {
      X[i] <- log(2-2*U, base = exp(1))
    }
    else {
      X[i] <- -1*log(2-2*U, base = exp(1))
    }
  }
  return(X)
}

results10 <- GenerateLaplace(1000)
min(results10); max(results10)
