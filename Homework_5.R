## Homework 5

## 1a

GammaConv <- function(iterations, alpha, beta) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    Y <- rexp(alpha, rate = beta)
    X <- append(X, sum(Y)); counter <- counter + 1
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

set.seed(255)
Results1a <- GammaConv(7500, 10, 4)

## Theoretical Mean and Variance for a Gamma(10, 4):
## Mean = (10)(4) = 40; Variance = (10)(4)^2 = 160
mean(Results1a$Output) > 40; var(Results1a$Output) > 160

hist(Results1a$Output, freq = FALSE)
X <- seq(0, max(Results1a$Output), .3); Gamma <- dgamma(X, 10, 4)
Gamma
lines(X, Gamma, col = 'red', lwd = 2)


## 1b

NegBinomConv <- function(iterations, r, p) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    Y <- rgeom(r, prob = p); Z <- sum(Y) + r
    X <- append(X, Z); counter <- counter + 1
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

set.seed(127)
Results1b <- NegBinomConv(7500, 5, .6)

## Theoretical Mean and Variance for a NegBinom(5, .6):
## Mean = (5)/(.6) = 8.33; Variance = (5)/(.6^)^2 = 500/36
mean(Results1b$Output) > 3.33; var(Results1b$Output) > 5.55

hist(Results1b$Output, breaks = seq(4.5, 30.5, by = 1), freq = FALSE)
X <- seq(0, max(Results1b$Output), 1); NegBi <- dnbinom(X, size = 5, prob = .6)
lines(X+5, NegBi, col = 'red', lwd = 2)

## 1c

ChisqConv <- function(iterations, v) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    Y <- rnorm(v); Z <- Y^2
    X <- append(X, sum(Z)); counter <- counter + 1
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

set.seed(329)
Results1c <- ChisqConv(7500, 8)
## Theoretical Mean and Variance for a Chisq(8):
## Mean = 8; Variance = 2(8) = 16
mean(Results1c$Output) > 8; var(Results1c$Output) > 16

hist(Results1c$Output, freq = FALSE)
X <- seq(0, max(Results1c$Output), .1); Chisq <- dchisq(X, 8)
lines(X, Chisq, col = 'red', lwd = 2)

################################################################################

## 2a

GammaSim <- function(iterations, r, s) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    U <- rgamma(1, r, rate = 1); V <- rgamma(1, s, rate = 1)
    X <- append(X, (U/(U+V))); counter <- counter + 1
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

Results2a <- GammaSim(1000, 2, 3)

hist(Results2a$Output, freq = F)
X <- seq(0, max(Results2a$Output), .1); Beta <- dbeta(X, 2, 3)
lines(X, Beta, col = 'red', lwd = 2)

## 2b

RBeta <- rbeta(1000, 2, 3)
qqplot(Results2a$Output, RBeta)

################################################################################

## 3a

BiNormPolar <- function(iterations) {
  X <- c(); Y <- c(); counter <- 0
  while (length(X) < iterations) {
    U_1 <- runif(1); U_2 <- runif(1)
    V_1 <- 2*U_1 - 1; V_2 <- 2*U_2 - 1
    if (V_1^2 + V_2^2 <= 1) {
      S <- V_1^2 + V_2^2
      X <- append(X, V_1*sqrt((-2*log(S))/S)); Y <- append(Y, V_2*sqrt((-2*log(S))/S))
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list("X" = X, "Y" = Y, "Proposals" = counter)
  return(mylist)
}

set.seed(404)
Results3a <- BiNormPolar(25000)

plot(Results3a$X, Results3a$Y)

## independence implies r close to 0
cor(Results3a$X, Results3a$Y)

## 3b

## The value of k that satisfies the above equation is k = 1.172

count <- 0
for (i in 1:length(Results3a$X)) {
  if (sqrt((Results3a$X[i])^2 + (Results3a$Y[i])^2) < 1.172) {
    count <- count + 1
  }
}
count/25000

## 3c

E.pi <- 4 / (Results3a$Proposals / 25000)
E.pi

################################################################################

## 4a

NormSim <- function(iterations) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    X_1 <- rnorm(1, 5, 2); X_2 <- rnorm(1, 10, 3); X_3 <- rnorm(1, 20, 5)
    X <- append(X, X_1 + X_2 + X_3)
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

set.seed(717)
Results4a <- NormSim(5000)
hist(Results4a$Output, freq = F)

## 4b

NormSimMix <- function(iterations) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    U <- runif(1)
    if (U < .5) {
      X_1 <- rnorm(1, 5, 2)
      X <- append(X, X_1)
      counter <- counter + 1
    }
    else if ((U >= .5) & (U < .8)) {
      X_2 <- rnorm(1, 10, 3)
      X <- append(X, X_2)
      counter <- counter + 1
    }
    else if ((U >= .8) & (U < 1)) {
      X_3 <- rnorm(1, 20, 5)
      X <- append(X, X_3)
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

set.seed(1917)
Results4b <- NormSimMix(5000)
hist(Results4b$Output, freq = F)

################################################################################

## 5

UnifTest <- function(iterations) {
  X <- c(); Y <- c(); counter <- 0
  while (length(X) < iterations) {
    U_1 <- runif(1); U_2 <- runif(1)
    V_1 <- 2*U_1 - 1; V_2 <- 2*U_2 - 1
    if (V_1^2 + V_2^2 <= 1) {
      R2 <- V_1^2 + V_2^2
      X <- append(X, R2); counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  mylist <- list("Output" = X, "Proposals" = counter)
  return(mylist)
}

set.seed(726)
R2 <- UnifTest(1000)

hist(R2$Output, freq = F)

################################################################################

## 6

library(VGAM)

## Rayleigh is chi with v = 2
## Generate 2 Z^2 and sum for chi^2 w/ v = 2
## Take sqrt of chi^2 for chi value
RayleighSamp <- function(iterations) {
  X <- c(); counter <- 0
  while (length(X) < iterations) {
    Y <- rnorm(2); Z <- sum(Y^2)
    X <- append(X, sqrt(Z)); counter <- counter + 1
  }
  mylist = list("Output" = X, "Proposals" = 2)
  return(mylist)
}

Results6 <- RayleighSamp(2500)

hist(Results6$Output, freq = F)
X <- seq(0, 20, 0.01); Rayleigh <- drayleigh(X, scale = 1)
lines(X, Rayleigh, col = 'red', lwd = 2)

################################################################################

## 7

## A = [0, 1] X [0, 1]; B = [2, 4] X [0, 1]
## A has Area = 1; B has Area = 2;
## Sample from A 1/3 of time; from B 2/3 of time
RectangleSamp <- function(iterations) {
  X <- c(); Y <- c(); counter <- 0
  while (length(X) < iterations) {
    U <- runif(1) 
    if (U <= (1/3)) {
      X_cord <- runif(1, 0, 1); Y_cord <- runif(1, 0, 1)
      X <- append(X, X_cord); Y <- append(Y, Y_cord)
      counter <- counter + 1
    }
    else if (U > (1/3) & (U < 1)) {
      X_cord <- runif(1, 2, 4); Y_cord <- runif(1, 0, 1)
      X <- append(X, X_cord); Y <- append(Y, Y_cord)
      counter <- counter + 1
    }
  }
  mylist <- list("X" = X, "Y" = Y, "Proposals" = counter)
  return(mylist)
}

Results7 <- RectangleSamp(2500)

plot(Results7$X, Results7$Y)

################################################################################

## 8 

## A = [0, 1] X [0, 2]; B = [1, 2] X [0, 3]; C = [2, 3] x [1, 3]
## A has Area = 2; B has Area = 3; C has Area = 2
## Sample from A 2/7 of time; from B 3/7 of time; C 2/7 of time
RectangleSamp2 <- function(iterations) {
  X <- c(); Y <- c(); counter <- 0
  while (length(X) < iterations) {
    U <- runif(1) 
    if (U <= (2/7)) {
      X_cord <- runif(1, 0, 1); Y_cord <- runif(1, 0, 2)
      X <- append(X, X_cord); Y <- append(Y, Y_cord)
      counter <- counter + 1
    }
    else if (U > (2/7) & (U <= 5/7)) {
      X_cord <- runif(1, 1, 2); Y_cord <- runif(1, 0, 3)
      X <- append(X, X_cord); Y <- append(Y, Y_cord)
      counter <- counter + 1
    }
    else if (U > (5/7) & (U < 1)) {
      X_cord <- runif(1, 2, 3); Y_cord <- runif(1, 1, 3)
      X <- append(X, X_cord); Y <- append(Y, Y_cord)
      counter <- counter + 1
    }
  }
  mylist <- list("X" = X, "Y" = Y, "Proposals" = counter)
  return(mylist)
}

set.seed(135)
Results8 <- RectangleSamp2(2500)

## Scatterplot of Samples
plot(Results8$X, Results8$Y)

################################################################################

## 9 

## Sample Uniformly from 2 Rectangles
## Let A = [-1, 1] x [0, .6]; B = [-.8, .8] x [.6, 1]
## Area of A = 2 * .6 = 1.2; Area of B = 1.6 * .4 = .64
## Sample from A = 1.2/1.84 of Time; From B = .64/1.84 of Time
SemiReject <- function(iterations) {
  X <- c(); Y <- c(); counter <- 0
  while (length(X) < iterations) {
    U <- runif(1)
    if (U < 1.2/1.84) {
      U_1 <- runif(1, -1, 1); U_2 <- runif(1, 0, .6)
      if (U_1^2 + U_2^2 <= 1) {
        X <- append(X, U_1); Y <- append(Y, U_2)
        counter <- counter + 1
      }
      else {
        counter <- counter + 1
      }
    }
    else {
      U_1 <- runif(1, -0.8, 0.8); U_2 <- runif(1, .6, 1)
      if (U_1^2 + U_2^2 <= 1) {
        X <- append(X, U_1); Y <- append(Y, U_2)
        counter <- counter + 1
      }
      else {
        counter <- counter + 1
      }
    }
  }
  mylist <- list("X" = X, "Y" = Y, "Proposals" = counter)
  return(mylist)
}

set.seed(140)
Results9 <- SemiReject(2500)

plot(Results9$X, Results9$Y)

2500/Results9$Proposals
