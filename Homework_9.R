## Homework 9

## 1

target_df <- read.delim("HW8_data.txt", header = F)
attach(target_df)

ConjPrior <- function(iterations, Y, k_0, mu_0, v_0, sigma2_0) {
  ## Initialize Output Vectors
  output.mu <- rep(0, iterations); output.sigma2 <- rep(0, iterations)
  
  ## Initialize Posterior Values
  n = length(Y)
  k_n = k_0 + n; mu_n = (k_0/k_n) * mu_0 + (n/k_n) * mean(Y); v_n = v_0 + n
  scale = v_0 * sigma2_0 + (n - 1) * var(Y) + (k_0 * n)/k_n * (mean(Y) - mu_0)^2
  
  for (i in 1:iterations) {
    ## Generate Next Set of Variables
    chi <- rchisq(1, v_n); chi.inv <- 1/chi; next_sigma2 <- chi.inv * scale
    next_mu <- rnorm(1, mu_n, (next_sigma2 / k_n))
    
    ## Assign to Output Vector
    output.mu[i] <- next_mu; output.sigma2[i] <- next_sigma2
  }
  mylist <- list("Mu" = output.mu, "Sigma.Sq" = output.sigma2)
  return(mylist)
}

## i

set.seed(517); results1.i1 <- ConjPrior(25000, target_df$V1, 0.1, 10, 2, 40)
set.seed(518); results1.i2 <- ConjPrior(25000, target_df$V1, 1, 10, 2, 40)
set.seed(519); results1.i3 <- ConjPrior(25000, target_df$V1, 10, 10, 2, 40)
set.seed(520); results1.i4 <- ConjPrior(25000, target_df$V1, 100, 10, 2, 40)

hist(results1.i1$Mu, main = "Post. Mu Dist (k_0 = 0.1)")
mean(results1.i1$Mu); quantile(results1.i1$Mu, c(0.025, 0.975))
mean(results1.i2$Mu); quantile(results1.i2$Mu, c(0.025, 0.975))
mean(results1.i3$Mu); quantile(results1.i3$Mu, c(0.025, 0.975))
mean(results1.i4$Mu); quantile(results1.i4$Mu, c(0.025, 0.975))

## ii

set.seed(521); results1.ii1 <- ConjPrior(25000, target_df$V1, 1, 20, 2, 40)
set.seed(522); results1.ii2 <- ConjPrior(25000, target_df$V1, 1, 20, 2, 80)
set.seed(523); results1.ii3 <- ConjPrior(25000, target_df$V1, 1, 20, 2, 160)
set.seed(524); results1.ii4 <- ConjPrior(25000, target_df$V1, 1, 20, 1, 2)
set.seed(525); results1.ii5 <- ConjPrior(25000, target_df$V1, 1, 20, 5, 2)
set.seed(526); results1.ii6 <- ConjPrior(25000, target_df$V1, 1, 20, 10, 2)


mean(results1.ii1$Sigma.Sq); quantile(results1.ii1$Sigma.Sq, c(0.025, 0.975))
mean(results1.ii2$Sigma.Sq); quantile(results1.ii2$Sigma.Sq, c(0.025, 0.975))
mean(results1.ii3$Sigma.Sq); quantile(results1.ii3$Sigma.Sq, c(0.025, 0.975))
mean(results1.ii4$Sigma.Sq); quantile(results1.ii4$Sigma.Sq, c(0.025, 0.975))
mean(results1.ii5$Sigma.Sq); quantile(results1.ii5$Sigma.Sq, c(0.025, 0.975))
mean(results1.ii6$Sigma.Sq); quantile(results1.ii6$Sigma.Sq, c(0.025, 0.975))

################################################################################

## 2

set.seed(629)
results2 <- ConjPrior(25000, target_df$V1, k_0 = 2, mu_0 = mean(target_df$V1),
                      v_0 = 2, sigma2_0 = var(target_df$V1))

results2.Yhat <- rep(0, length(results2$Mu)); set.seed(630)
for (i in 1:length(results2.Yhat)) {
  results2.Yhat[i] = rnorm(1, results2$Mu[i], sqrt(results2$Sigma.Sq[i]))
}

hist(results2.Yhat, freq = F)
mean(results2.Yhat)
quantile(results2.Yhat, c(0.025, 0.975)); quantile(results2.Yhat, c(0.005, 0.995))

################################################################################

## 3 

School1 <- read.delim("School1.txt", header = F)
School2 <- read.delim("School2.txt", header = F)
School3 <- read.delim("School3.txt", header = F)

set.seed(317); results3.S1 <- ConjPrior(5000, School1$V1, 1, 5, 2, 4)
set.seed(318); results3.S2 <- ConjPrior(5000, School2$V1, 1, 5, 2, 4)
set.seed(319); results3.S3 <- ConjPrior(5000, School3$V1, 1, 5, 2, 4)

results3.S1.Yhat <- rep(0, length(results3.S1$Mu)); set.seed(329)
for (i in 1:length(results3.S1.Yhat)) {
  results3.S1.Yhat[i] = rnorm(1, results3.S1$Mu[i], sqrt(results3.S1$Sigma.Sq[i]))
}

results3.S2.Yhat <- rep(0, length(results3.S2$Mu)); set.seed(330)
for (i in 1:length(results3.S2.Yhat)) {
  results3.S2.Yhat[i] = rnorm(1, results3.S2$Mu[i], sqrt(results3.S2$Sigma.Sq[i]))
}

results3.S3.Yhat <- rep(0, length(results3.S3$Mu)); set.seed(331)
for (i in 1:length(results3.S3.Yhat)) {
  results3.S3.Yhat[i] = rnorm(1, results3.S3$Mu[i], sqrt(results3.S3$Sigma.Sq[i]))
}

## 3a

mean(results3.S1$Mu); quantile(results3.S1$Mu, c(0.025, 0.975)) ## School 1
mean(results3.S1$Sigma.Sq); quantile(results3.S1$Sigma.Sq, c(0.025, 0.975))

mean(results3.S2$Mu); quantile(results3.S2$Mu, c(0.025, 0.975)) ## School 2
mean(results3.S2$Sigma.Sq); quantile(results3.S2$Sigma.Sq, c(0.025, 0.975))

mean(results3.S3$Mu); quantile(results3.S3$Mu, c(0.025, 0.975)) ## School 3
mean(results3.S3$Sigma.Sq); quantile(results3.S3$Sigma.Sq, c(0.025, 0.975))

## 3b

mean((results3.S1$Mu < results3.S2$Mu) & (results3.S2$Mu < results3.S3$Mu))
mean((results3.S1$Mu < results3.S3$Mu) & (results3.S3$Mu < results3.S2$Mu))
mean((results3.S2$Mu < results3.S1$Mu) & (results3.S1$Mu < results3.S3$Mu))
mean((results3.S2$Mu < results3.S3$Mu) & (results3.S3$Mu < results3.S1$Mu))
mean((results3.S3$Mu < results3.S1$Mu) & (results3.S1$Mu < results3.S2$Mu))
mean((results3.S3$Mu < results3.S2$Mu) & (results3.S2$Mu < results3.S1$Mu))

## 3c

mean((results3.S1.Yhat < results3.S2.Yhat) & (results3.S2.Yhat < results3.S3.Yhat))
mean((results3.S1.Yhat < results3.S3.Yhat) & (results3.S3.Yhat < results3.S2.Yhat))
mean((results3.S2.Yhat < results3.S1.Yhat) & (results3.S1.Yhat < results3.S3.Yhat))
mean((results3.S2.Yhat < results3.S3.Yhat) & (results3.S3.Yhat < results3.S1.Yhat))
mean((results3.S3.Yhat < results3.S1.Yhat) & (results3.S1.Yhat < results3.S2.Yhat))
mean((results3.S3.Yhat < results3.S2.Yhat) & (results3.S2.Yhat < results3.S1.Yhat))

## 3d

(mean((results3.S3$Mu < results3.S2$Mu) & (results3.S2$Mu < results3.S1$Mu)) 
  + mean((results3.S2$Mu < results3.S3$Mu) & (results3.S3$Mu < results3.S1$Mu)))

(mean((results3.S2.Yhat < results3.S3.Yhat) & (results3.S3.Yhat < results3.S1.Yhat))
  + mean((results3.S3.Yhat < results3.S2.Yhat) & (results3.S2.Yhat < results3.S1.Yhat)))

################################################################################

## 4

EmInEm <- function(iterations, theta_1, theta_2, theta_3, theta_4, theta_5, theta_6) {
  theta1.out <- rep(0, iterations); theta2.out <- rep(0, iterations);
  theta3.out <- rep(0, iterations); theta4.out <- rep(0, iterations);
  theta5.out <- rep(0, iterations); theta6.out <- rep(0, iterations);
  
  for (i in 1:iterations) {
    ## Generate Xs
    x_1 <- rgamma(1, theta_1, rate = 1); x_2 <- rgamma(1, theta_2, rate = 1)
    x_3 <- rgamma(1, theta_3, rate = 1); x_4 <- rgamma(1, theta_4, rate = 1)
    x_5 <- rgamma(1, theta_5, rate = 1); x_6 <- rgamma(1, theta_6, rate = 1)
    x_sum = x_1 + x_2 + x_3 + x_4 + x_5 + x_6
    
    ## Generate Thetas
    t_1 = x_1/x_sum; t_2 = x_2/x_sum; t_3 = x_3/x_sum; t_4 = x_4/x_sum
    t_5 = x_5/x_sum; t_6 = x_6/x_sum
    
    ## Assign to Output Vector
    theta1.out[i] <- t_1; theta2.out[i] <- t_2; theta3.out[i] <- t_3
    theta4.out[i] <- t_4; theta5.out[i] <- t_5; theta6.out[i] <- t_6
  }
  mylist <- list("theta_red" = theta1.out, "theta_orange" = theta2.out,
                 "theta_yellow" = theta3.out, "theta_green" = theta4.out,
                 "theta_blue" = theta5.out, "theta_brown" = theta6.out)
  return(mylist)
}

## 4a

set.seed(449)
results4a <- EmInEm(1000, 11 + 0, 1 + 0, 7 + 0, 3 + 0, 4 + 0, 6 + 0)
mean(results4a$theta_red); quantile(results4a$theta_red, c(0.025, 0.975)) 

## 4b 

set.seed(450)
results4b <- EmInEm(1000, 11 + 1, 1 + 1, 7 + 1, 3 + 1, 4 + 1, 6 + 1)
mean(results4b$theta_red); quantile(results4b$theta_red, c(0.025, 0.975)) 

## 4c

set.seed(451)
results4c <- EmInEm(1000, 11 + 10, 1 + 10, 7 + 10, 3 + 10, 4 + 10, 6 + 10)
mean(results4c$theta_red); quantile(results4c$theta_red, c(0.025, 0.975)) 

## 4d

set.seed(452)
results4d <- EmInEm(1000, 11 + 13, 1 + 21, 7 + 13, 3 + 20, 4 + 21, 6 + 12)
mean(results4d$theta_red); quantile(results4d$theta_red, c(0.025, 0.975))

## Set Densities
red_density <- density(results4d$theta_red)
orange_density <- density(results4d$theta_orange)
yellow_density <- density(results4d$theta_yellow)
green_density <- density(results4d$theta_green)
blue_density <- density(results4d$theta_blue)
brown_density <- density(results4d$theta_brown)

## Construct Plot
plot(red_density$x, red_density$y, type = 'l', col = 'red', lwd = 2, 
     xlab = "Label X", ylab = "Density", main = "M&M Colors")
lines(orange_density$x, orange_density$y, type = 'l', col = 'orange', lwd = 2)
lines(yellow_density$x, yellow_density$y, type = 'l', col = 'yellow', lwd = 2)
lines(green_density$x, green_density$y, type = 'l', col = 'green', lwd = 2)
lines(blue_density$x, blue_density$y, type = 'l', col = 'blue', lwd = 2)
lines(brown_density$x, brown_density$y, type = 'l', col = 'brown', lwd = 2)
