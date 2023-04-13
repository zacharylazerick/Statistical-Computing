## Homework 7

GoatorNoGoat <- function(iterations, num.doors = c(1, 2, 3), switch = T) {
  output <- rep(0, iterations); counter <- 1
  while (counter < iterations) {
    ## Pick Door for Car
    CarDoor <- sample(num.doors, size = 1)
    
    ## Pick Door for Contestant
    ContestantDoor <- sample(num.doors, size = 1)
    
    ## Pick Door for Monty
    MontyDoor <- sample(setdiff(num.doors, union(CarDoor, ContestantDoor)), size = length(num.doors)-2, replace = F)
    
    ## If Switch = T, Contestant Switches Their Choice
    if (switch == T) {
      ContestantDoor <- num.doors[-c(MontyDoor, ContestantDoor)]
    }
    
    ## Check if Contestant Picked Car
    if (ContestantDoor == CarDoor) {
      output[counter] <- 1
      counter <- counter + 1
    }
    else {
      counter <- counter + 1
    }
  }
  return(output)
}

set.seed(732); results1a.1 <- GoatorNoGoat(1000); 
set.seed(1432); results1a.2 <- GoatorNoGoat(1000, switch = F)
mean(results1a.1); mean(results1a.2)

## 1b 
set.seed(732); results1b.1 <- GoatorNoGoat(1000, num.doors = 1:10); 
set.seed(1432); results1b.2 <- GoatorNoGoat(1000, num.doors = 1:10, switch = F)
mean(results1b.1); mean(results1b.2)

#################################################################################

## Likelihood ~ Binomial(n = 30, p = 6/30) --> 6 success, 24 failure

## 2a (Unif(0, 1))

num.samp = 1000; set.seed(1001)
Posterior2a <- rbeta(num.samp, 6+1, 24 + 1)

hist(Posterior2a, freq = F)
X <- seq(0, 1, .01); Beta <- dbeta(X, 6+1, 24+1)
lines(X, Beta, col = 'red', lwd = 2)

mean(Posterior2a)

quantile(Posterior2a, c(0.025, 0.975))

## 2b (Beta(2, 6))

set.seed(1002)
Posterior2b <- rbeta(num.samp, 6+1, 24 + 1)

hist(Posterior2b, freq = F)
X <- seq(0, 1, .01); Beta <- dbeta(X, 6+1, 24+1)
lines(X, Beta, col = 'red', lwd = 2)

mean(Posterior2b)

quantile(Posterior2b, c(0.025, 0.975))

## 2c (Beta(6, 2))

set.seed(1000)
Posterior2c <- rbeta(num.samp, 6+1, 24 + 1)

hist(Posterior2c, freq = F)
X <- seq(0, 1, .01); Beta <- dbeta(X, 6+1, 24+1)
lines(X, Beta, col = 'red', lwd = 2)

mean(Posterior2c)

quantile(Posterior2c, c(0.025, 0.975))

## 2d (Beta(20, 60))

set.seed(1000)
Posterior2d <- rbeta(num.samp, 6+1, 24 + 1)

hist(Posterior2d, freq = F)
X <- seq(0, 1, .01); Beta <- dbeta(X, 6+1, 24+1)
lines(X, Beta, col = 'red', lwd = 2)

mean(Posterior2d)

quantile(Posterior2d, c(0.025, 0.975))

#################################################################################

## 3a (Gamma(1, 1))

## Likelihood --> n = 20, sum.x = 74.302

set.seed(1005)
Posterior3a <- rgamma(num.samp, 20 + 1, 74.302 + 1)

hist(Posterior3a, freq = F)
X <- seq(0, 1, .01); Gamma <- dgamma(X, 20 + 1, 74.302+1)
lines(X, Gamma, col = 'red', lwd = 2)

mean(Posterior3a)

quantile(Posterior3a, c(0.025, 0.975))

## 3b (Gamma(10, 10))

set.seed(1006)
Posterior3b <- rgamma(num.samp, 20 + 10, 74.302 + 10)

hist(Posterior3b, freq = F)
X <- seq(0, 1, .01); Gamma <- dgamma(X, 20 + 10, 74.302+10)
lines(X, Gamma, col = 'red', lwd = 2)

mean(Posterior3b)

quantile(Posterior3b, c(0.025, 0.975))

## 3c (Gamma(1, 4))

set.seed(1007)
Posterior3c <- rgamma(num.samp, 20 + 1, 74.302 + 4)

hist(Posterior3c, freq = F)
X <- seq(0, 1, .01); Gamma <- dgamma(X, 20 + 1, 74.302+4)
lines(X, Gamma, col = 'red', lwd = 2)

mean(Posterior3c)

quantile(Posterior3c, c(0.025, 0.975))

## 3d (Gamma(4, 16))

set.seed(1008)
Posterior3d <- rgamma(num.samp, 20 + 4, 74.302 + 16)

hist(Posterior3d, freq = F)
X <- seq(0, 1, .01); Gamma <- dgamma(X, 20 + 4, 74.302+16)
lines(X, Gamma, col = 'red', lwd = 2)

mean(Posterior3d)

quantile(Posterior3d, c(0.025, 0.975))

## 3e

set.seed(1140)
results3e <- rgamma(1000, 20 + 4, 74.302 + 16)

## 3f

results3f <- 1/results3e
  
## 3g

mean(results3f > 4)

################################################################################

## 4a 

## Prior --> Gamma(47.6, 2) (Averaged 23.8 fa/year, add 2 years of 'past' data)

set.seed(1036)
Posterior4a <- rgamma(num.samp, 238 + 47.6, 10 + 2)

mean(Posterior4a)

quantile(Posterior4a, c(0.025, 0.975))

## 4b

results4b <- rnbinom(num.samp, size = 238 + 47.6, prob = (10+2)/(10+2+1))

quantile(results4b, c(0.025, 0.975))

## 4c

## Prior --> Gamma(1383.8, 2) (691.9 Averaged pd/year, add 2 years of 'past' data)

set.seed(1145)
Posterior4c <- rgamma(num.samp, 6919 + 1383.8, 10 + 2)

mean(Posterior4c)

quantile(Posterior4c, c(0.025, 0.975))

results4c <- rnbinom(1000, size = 6919 + 1383.8, prob = (10+2)/(10+2+1))

quantile(results4c, c(0.025, 0.975))

##################################################################################

## 5a Uniform --> Beta(1, 1)

## Binomial Likelihood --> Binom(100, 58/100)

set.seed(1317)
Posterior5a <- rbeta(num.samp, 58 + 1, 42 + 1)

mean(Posterior5a); var(Posterior5a)

quantile(Posterior5a, c(0.025, 0.975))

## 5b Prior --> Beta(58+1, 42+1)

set.seed(1348)
Posterior5b <- rbeta(num.samp, 57 + 58 + 1, 43 + 42 + 1)

mean(Posterior5b); var(Posterior5b)

quantile(Posterior5b, c(0.025, 0.975))

## 5c --> NY ~ Beta(30 + 1, 20 + 1), MA ~ Beta(58 + 1, 42 + 1)

set.seed(1350)
PosteriorNY <- rbeta(num.samp, 30 + 1, 20 + 1); PosteriorMA <- rbeta(num.samp, 58 + 1, 42 + 1)

mean(PosteriorMA < PosteriorNY)

################################################################################

## 6b 

## Prior Ratio 6:1, add 2 'past' games --> Gamma(12, 2)

set.seed(1402)
Posterior6b <- rgamma(num.samp, 12 + 58, 2 + 12)

mean(Posterior6b); var(Posterior6b)

quantile(Posterior6b, c(0.025, 0.975))

## 6c

set.seed(1404)
Theta6c <- rgamma(num.samp, 12 + 58, 2 + 12)
Poisson6c <- rpois(num.samp, Theta6c)

table(Poisson6c)
mean(Poisson6c)

## 6d

set.seed(1420)
Posterior6d <- rgamma(num.samp, 12 + 80, 2 + 12)

mean(Posterior6d); var(Posterior6d)

quantile(Posterior6d, c(0.025, 0.975))

## 6e 
## ''6e Refer to Maryland, ''6c Refer to Holy Cross

set.seed(1424)
Theta6e <- rgamma(num.samp, 12 + 80, 2 + 12)
Poisson6e <- rpois(num.samp, Theta6e)

DiffTheta <- Theta6c - Theta6e; DiffPoisson <- Poisson6c - Poisson6e

hist(DiffTheta); hist(DiffPoisson)

quantile(DiffTheta, c(0.025, 0.975)); quantile(DiffPoisson, c(0.025, 0.975))
