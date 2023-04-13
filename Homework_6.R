## Homework 6

## 1

MultivariateNormal <- function(iterations, mu, Sigma) {
  X <- matrix(data = NA, nrow = iterations, ncol = 3, byrow = T)
  A <- chol(Sigma)
  for (i in 1:iterations) {
    Z <- rnorm(3)
    X[i, 1:3] <- t(Z%*%A + mu)
  }
  return(X)
}

set.seed(1838)
mu <- c(1, 2, 3); Sigma <- matrix(c(3, -2, 1, -2, 5, 3, 1, 3, 5), nrow = 3, ncol = 3, byrow = T)
results1 <- MultivariateNormal(1000, mu, Sigma)

################################################################################

## 2

StandardizeNormal <- function(matrix) {
  ## Find and Invert and Upper-Triangular Matrix A such that Sigma = A^T*A
  A <- chol(cov(matrix)); A.inverse <- solve(A)
  ## Compute Vector of Means
  Mu <- colMeans(matrix); 
  
  ## Compute Z
  for (i in 1:nrow(matrix)) {
    Z[i, 1:ncol(matrix)] <- (matrix[i, 1:ncol(matrix)] - Mu)%*%A.inverse
  }
  return(Z)
}

results2 <- StandardizeNormal(results1)

colMeans(results2)

cov(results2)

################################################################################

## 3a

BivariateBurnIn <- function(iterations, initial_x, initial_y, rho, lag = 1) {
  X.cord <- c(); Y.cord <- c(); counter <- 0
  next_x <- initial_x; next_y <- initial_y
  while (length(X.cord) < iterations) {
    next_x = rnorm(1, mean = rho*next_y, sd = sqrt(1-rho^2))
    next_y = rnorm(1, mean = rho*next_x, sd = sqrt(1-rho^2))
    if (counter %% lag == 0) {
      X.cord <- append(X.cord, next_x); Y.cord <- append(Y.cord, next_y)
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list("X" = X.cord, "Y" = Y.cord)
  return(mylist)
}

set.seed(518)
results3 <- BivariateBurnIn(500, 80, 80, .75)

plot(results3$X[1:51], results3$Y[1:51])
arrows(x0 = results3$X[1:50], y0 = results3$Y[1:50], 
       x1 = results3$X[2:51], y1 = results3$Y[2:51], col='red')

## 3b

plot(results3$X, results3$Y)

## 3d

acf(results3$X); acf(results3$Y)

## 3e

set.seed(520)
results3e <- BivariateBurnIn(10000, 0, 0, .75, lag = 5)

## Check Autocorrelation of Simulated X
acf(results3e$X)

## Check Mean, Variance of Simulated X
mean(results3e$X); var(results3e$X)

## Check Q-Q Plot
quantile(results3e$X, probs = (seq(0, 1, .1)))
qnorm(seq(0, 1, .1))

################################################################################

## 4a

rho <- seq(-1, 1, .1)
freq <- rep(0, length(rho))

set.seed(540)
for (i in 1:length(rho)) {
  results4 <- BivariateBurnIn(15000, 0, 0, rho[i], lag = 5)
  counter <- 0
  for (j in 1:length(results4$X))
    if ((results4$X[j] > 0) & (results4$Y[j] > 0)) {
      counter <- counter + 1
    }
  freq[i] <- counter/length(results4$X)
}

plot(rho, freq, xlim = c(-1, 1), ylim = c(0, 1), 
     type = 'l', col = 'red', lwd = 2)

## 4b

set.seed(545)
for (i in 1:length(rho)) {
  results4 <- BivariateBurnIn(15000, 0, 0, rho[i], lag = 5)
  counter <- 0
  for (j in 1:length(results4$X))
    if ((results4$X[j] < 0) & (results4$Y[j] > 0)) {
      counter <- counter + 1
    }
  freq[i] <- counter/length(results4$X)
}

plot(rho, freq, xlim = c(-1, 1), ylim = c(0, 1), 
     type = 'l', col = 'red', lwd = 2)

## 4c

set.seed(550)
for (i in 1:length(rho)) {
  results4 <- BivariateBurnIn(15000, 0, 0, rho[i], lag = 5)
  counter <- 0
  for (j in 1:length(results4$X))
    if (results4$X[j] > 0) {
      counter <- counter + 1
    }
  freq[i] <- counter/length(results4$X)
}

plot(rho, freq, xlim = c(-1, 1), ylim = c(0, 1), 
     type = 'l', col = 'red', lwd = 2)

################################################################################

## 5a 

SliceSampler <- function(iterations, initial_x, initial_y) {
  X <- c(); Y <- c()
  while (length(X) < iterations) {
    ## Initialize Values
    next_x <- initial_x; next_y <- initial_y
    
    ## Generate New Point
    next_x <- runif(1, next_y - 1, next_y + 1)
    next_y <- runif(1, next_x - 1, next_x + 1)
    
    ## If New Point is Valid, Append
    if ((abs(next_x - next_y) <= 1) & (next_x >= 0 & next_y >= 0) & (next_x <= 100 & next_y <= 100)) {
      X <- append(X, next_x); Y <- append(Y, next_y)
      initial_x <- next_x; initial_y <- next_y
    }
  }
  mylist <- list("X_cord" = X, "Y_cord" = Y)
  return(mylist)
  }

set.seed(500)
results5a <- SliceSampler(10000, 50, 50)

plot(results5a$X_cord)

################################################################################

## 6

BetaBinom <- function(iterations, initial_x, initial_y, a = 2, b = 4, n = 50, burn_in = 5, lag = 9) {
  X <- c(); Y <- c(); counter <- 0
  next_x <- initial_x; next_y <- initial_y
  while (length(X) < iterations) {
    next_x <- rbinom(1, size = n, prob = next_y)
    next_y <- rbeta(1, next_x + a, n - next_x + b)
    if ((counter > burn_in) & (counter %% lag == 0)) {
      X <- append(X, next_x); Y <- append(Y, next_y)
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list("X_cord" = X, "Y_cord" = Y)
  return(mylist)
}

set.seed(405)
results6 <- BetaBinom(100, 50, (100/101))

plot(results6$X_cord, results6$Y_cord)
arrows(x0 = results6$X_cord[1:50], y0 = results6$Y_cord[1:50], 
       x1 = results6$X_cord[2:51], y1 = results6$Y_cord[2:51], col='red')

acf(results6$X_cord); acf(results6$Y_cord)

mean(results6$X_cord); mean(results6$Y_cord)

