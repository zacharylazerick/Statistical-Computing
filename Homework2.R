## Homework 2

## Problem 1

Fibonacci <- function(x) {
  Fibo_list <- c(1, 1)
  while(length(Fibo_list) < x) {
    Fibo_list[length(Fibo_list)+1] <- Fibo_list[length(Fibo_list)-1] + 
      Fibo_list[length(Fibo_list)]
  }
  return(Fibo_list)
}

## Test for 1
Fibonacci(100)

## Problem 2

PrimeFinder <- function(x, y) {
  i <- x; prime_list <- c();
  while (i <= y) {
    j <- 1; factor_list <- c();
      while (j <= i) {
        if (i %% j == 0) {
          factor_list <- append(factor_list, j)
          j <- j + 1
        }
        else {
          j <- j + 1
        }
      }
    if (length(factor_list) == 2) {
      prime_list <- append(prime_list, i)
      i <- i + 1
    }
    else {
      i <- i + 1
    }
  }
  cat("The prime numbers from", x, "to", y, "are", prime_list)
}

## Test for 2

PrimeFinder(1, 100)
PrimeFinder(1, 500)
PrimeFinder(1, 1000)

## Problem 4

hist(NFL_Offense$PYds.G, col = 'red', breaks = 16,
     main = "Total Yards per Game", 
     xlab = "Yards", ylab = "Frequency")
hist(NFL_Offense$RYds.G, add = T, col = 'blue', breaks = 16)
     
plot(NFL_Offense$Pts.G, (NFL_Offense$PYds.G + NFL_Offense$RYds.G),
     xlab = "Points per Game", ylab = "Total Yards per Game")

Pts_TotYds.G.lm <- lm((NFL_Offense$PYds.G + NFL_Offense$RYds.G)~
                        NFL_Offense$Pts.G)
abline(Pts_TotYds.G.lm, col = 'red', lwd = '2')

summary(Pts_TotYds.G.lm)$r.squared

plot(NFL_Offense$Pts.G, NFL_Defense$Pts.G, 
     main = "Points Scored vs. Points Allowed (per Game)", 
     xlab = "Points Scored", ylab = "Points Allowed")

## Build Regression Model
Pts_Scored_Allowed.G.lm <- lm(NFL_Defense$Pts.G~NFL_Offense$Pts.G)

## Report $R^2$ Value
summary(Pts_Scored_Allowed.G.lm)$r.squared

boxplot(NFL_Offense$Pts.G, NFL_Defense$Pts.G, col = c('red', 'blue'),
        main = "Points Scored per Game",
        xlab = "Offense                               Defense", 
        ylab = "Points")

## Code for 5

LCG <- function(n, m, a, b, x) {
  list_X <- c(x)
  while (length(list_X) <= n) {
    x <- list_X[length(list_X)]
    Next_X <- (a * x + b) %% m
    list_X <- append(list_X, Next_X)
  }
  print(list_X)
}

LCG(10, 8, 5, 1, 2)

X <- runif(1000)
plot(X[1:999], X[2:1000], asp=1)

m <- 81; a <- 1; c <- 8; x <- 0
X <- LCG(1000, m, a, c, x)/m
plot(X[1:999], X[2:1000], asp=1)

m <- 232; a <- 1664525; b <- 1013904223; x <- 0
X <- LCG(1000, m, a, b, x)/m

X_list <- c(23, 66)
while (length(X_list) < 15) {
  Next_X <- (3 * X_list[length(X_list) - 1] + 5 *
               X_list[length(X_list)]) %% 100
  X_list <- append(X_list, Next_X)
}

U_list <- X_list / 100

U_list


X <- runif(1000)
Y <- rep(1, 1000)

for (i in 1:length(X)) {
  if ((X[i] >= 0) & (X[i] < .3)) {
    Y[i] <- 1
  }
  else if ((X[i] >= .3) & (X[i] < .5)) {
    Y[i] <- 2
  }
  else if ((X[i] >= .5) & (X[i] < .85)) {
    Y[i] <- 3
  }
  else if ((X[i] >= .85) & (X[i] < 1)) {
    Y[i] <- 4
  }
  else {
    Y[i] <- 0
  }
}

## Count how many iterations have 3 empty urns
count = 0; set.seed(2047)

## Simulate 1000 Times
for (i in 1:1000) {
  X <- sample(x = c(1, 2, 3, 4, 5, 6, 7, 8), size = 9, replace = T)
  Urn_list <- c()
  for (j in 1:length(X)) {
    ## Check if Urn_list already
    if (X[j] %in% Urn_list == F) {
      Urn_list <- append(Urn_list, X[j])
      j <- j + 1
    }
    else {
      j <- j + 1
    }
  }
  ## If length == 5, then 3 urns are empty
  if (length(Urn_list) == 5) {
    count = count + 1
  }
}

##Report Probability of Exactly 3 Empty Urns
count/1000

## Define Storage Vector
N <- rep(0, 100)

for (i in 1:100) {
  Sum = 0; count = 0; X <- runif(1000)
  while (Sum < 1) {
    count = count + 1; Sum = Sum + X[count]
  }
  N[i] <- count
}

sum(N)/100

## Code for 8
exp(-3)

N <- c(); 

for (i in 1:100) {
  n <- 1; prod <- 1; X <- runif(1000);
  while (prod >= exp(-3)) {
    prod <- prod * X[n]
    n <- n + 1
  }
  N <- append(N, n)
}

N
sum(N)/100
