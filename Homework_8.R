## Homework 8


## 1

Craps <- function(iterations, first_roll) {
  output <- c(); rolls <- c()
  while (length(output) < iterations) {
    sum <- 0; num_rolls <- 0
    while (sum != 7) {
      d_1 <- sample(1:6, size = 1); d_2 <- sample(1:6, size = 1)
      sum <- d_1 + d_2; num_rolls <- num_rolls + 1
      if (sum == first_roll) {
        output <- append(output, 1)
        rolls <- append(rolls, num_rolls)
        break
      }
      if (sum == 7) {
        output <- append(output, 0)
        rolls <- append(rolls, num_rolls)
        break
      }
    }
  }
  mylist <- list("Output" = output, "Rolls" = rolls)
  return(mylist)
}

set.seed(855)
results1 <- Craps(1000, first_roll = 8)

## 1a

mean(results1$Output)

## 1b

mean(results1$Output == 1 & results1$Rolls == 1)/mean(results1$Output)

## 1c

mean(results1$Rolls)

################################################################################

## 2 (i)

num.samp <- 10000; n <- 21; s2 <- 25
set.seed(1249)
gamma2i <- rgamma(10000, (n-1)/2, (1/2)*(n-1)*s2)
results2i <- 1/gamma2i

hist(results2i, freq = F)
mean(results2i)
quantile(results2i, c(0.025, 0.975))

## 2 (ii)

set.seed(1250)
unif2ii <- runif(num.samp)
gamma2ii <- qgamma(unif2ii, (n-1)/2, (1/2)*(n-1)*s2)
results2ii <- 1/gamma2ii

hist(results2ii, freq = F)
mean(results2ii)
quantile(results2ii, c(0.025, 0.975))

## 2 (iii)

set.seed(1251)
chisq2iii <- rchisq(num.samp, n-1)
invchisq2iii <- 1/chisq2iii
results2iii <- invchisq2iii*(n-1)*s2

hist(results2iii, freq = F)
mean(results2iii)
quantile(results2iii, c(0.025, 0.975))

################################################################################

target_df <- read.delim("HW8_data.txt", header = F)
attach(target_df)

## 3 (i)

num.samp <- 25000; n = 45

results3i.mu <- rep(0, num.samp); results3i.sigma <- rep(0, num.samp)

## Initialize Sigma
next_sigma <- var(V1); y_bar <- mean(V1); s2 <- var(V1)

set.seed(1821)
for (i in 1:num.samp) {
  next_mu <- rnorm(1, y_bar, sqrt(next_sigma/n))
  gamma <- rgamma(1, n/2, (1/2)*((n-1)*s2 + n*(y_bar - next_mu)^2))
  next_sigma <- 1/gamma
  
  results3i.mu[i] <- next_mu; results3i.sigma[i] <- next_sigma
}

plot(results3i.mu[1:49], results3i.mu[2:50])
arrows(x0 = results3i.mu[1:25], y0 = results3i.mu[2:26], 
       x1 = results3i.mu[2:26], y1 = results3i.mu[3:27], col='red')
acf(results3i.mu)

## Mu Statistics
hist(results3i.mu, freq = F)
quantile(results3i.mu, c(0.025, 0.975)) ## 95% CI
quantile(results3i.mu, c(0.005, 0.995)) ## 99% CI

## Sigma Statistics
hist(results3i.sigma, freq = F)
quantile(results3i.sigma, c(0.025, 0.975)) ## 95% CI
quantile(results3i.sigma, c(0.005, 0.995)) ## 99% CI

## 3 (ii)

results3ii.mu <- rep(0, num.samp); results3ii.sigma <- rep(0, num.samp)

## Initialize Sigma
next_sigma = sd(V1); y_bar <- mean(V1); s2 <- var(V1)

set.seed(1835)
for (i in 1:num.samp) {
  gamma <- rgamma(1, (n-1)/2, (1/2)*(n-1)*s2)
  next_sigma <- 1/gamma
  next_mu <- rnorm(1, y_bar, sqrt(next_sigma/n))
  
  results3ii.mu[i] <- next_mu; results3ii.sigma[i] <- next_sigma
}

## Mu Statistics
hist(results3ii.mu, freq = F)
quantile(results3ii.mu, c(0.025, 0.975)) ## 95% CI
quantile(results3ii.mu, c(0.005, 0.995)) ## 99% CI

## Sigma Statistics
hist(results3ii.sigma, freq = F)
quantile(results3ii.sigma, c(0.025, 0.975)) ## 95% CI
quantile(results3ii.sigma, c(0.005, 0.995)) ## 99% CI

################################################################################

## 4a

Y <- c(9.0, 8.5, 7.0, 8.5, 6.0, 12.5, 6.0, 9.0, 8.5, 7.5, 8.0, 6.0, 9.0, 8.0, 
       7.0, 10.0, 9.0, 7.5, 5.0, 6.5)
y_bar <- mean(Y); s2 <- var(Y); n <- length(Y)

set.seed(721)
results4a.mu <- rep(0, 1000); results4a.sigma <- rep(0, 1000)
for (i in 1:length(results4a.mu)) {
  gamma <- rgamma(1, (n-1)/2, (1/2)*(n-1)*s2)
  sigma <- 1/gamma
  mu <- rnorm(1, y_bar, sqrt(sigma/n))
  results4a.mu[i] <- mu; results4a.sigma[i] <- sigma
}

## 4b

quantile(results4a.mu, c(0.05, 0.95)) ## 90% CI for Mu
quantile(results4a.sigma, c(0.05, 0.95)) ## 90% CI for Sigma

## 4c

p_75 <- results4a.mu + 0.674 * results4a.sigma
mean(p_75); sd(p_75)

################################################################################

## 5a

yj_males <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
yj_females <- c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)

m.y_bar <- mean(yj_males); m.s2 <- var(yj_males); m.n <- length(yj_males)
f.y_bar <- mean(yj_females); f.s2 <- var(yj_females); f.n <- length(yj_females)

set.seed(734)
next_sigma <- m.s2
yj_males_results.mu <- rep(0, 1000); yj_males_results.sigma <- rep(0, 1000)
for (i in 1:length(yj_males_results.mu)) {
  next_mu <- rnorm(1, m.y_bar, sqrt(next_sigma/m.n))
  gamma <- rgamma(1, (m.n/2), (1/2)*((m.n - 1)*m.s2 + m.n*(m.y_bar - next_mu)^2))
  next_sigma <- 1/gamma
  
  yj_males_results.mu[i] <- next_mu; yj_males_results.sigma[i] <- next_sigma
}

plot(yj_males_results.mu[1:999], yj_males_results.mu[2:1000])
arrows(x0 = yj_males_results.mu[1:25], y0 = yj_males_results.mu[2:26], 
       x1 = yj_males_results.mu[2:26], y1 = yj_males_results.mu[3:27], col='red')
acf(yj_males_results.mu) 

set.seed(742)
next_sigma <- f.s2
yj_females_results.mu <- rep(0, 1000); yj_females_results.sigma <- rep(0, 1000)
for (i in 1:length(yj_females_results.mu)) {
  next_mu <- rnorm(1, f.y_bar, sqrt(next_sigma/f.n))
  gamma <- rgamma(1, (f.n/2), (1/2)*((f.n - 1)*f.s2 + f.n*(f.y_bar - next_mu)^2))
  next_sigma <- 1/gamma
  
  yj_females_results.mu[i] <- next_mu; yj_females_results.sigma[i] <- next_sigma
}

plot(yj_females_results.mu[1:999], yj_females_results.mu[2:1000])
arrows(x0 = yj_females_results.mu[1:25], y0 = yj_females_results.mu[2:26], 
       x1 = yj_females_results.mu[2:26], y1 = yj_females_results.mu[3:27], col='red')
acf(yj_females_results.mu) 

## 5c

yj_diff.mu <- yj_males_results.mu - yj_females_results.mu

hist(yj_diff.mu, freq = F)
mean(yj_diff.mu)
quantile(yj_diff.mu, c(0.025, 0.975))

## 5d

mean(yj_males_results.mu > yj_females_results.mu)
