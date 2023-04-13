## Homework 1 Script

## 1

Reverse <- function(x) {
  b <- rep(0, length(x))
  for (i in 1:length(x)) {
    b[i] <- x[(length(x)+1) - i]
  }
  return(b)
}

## Test for 1
x <- sample(1:10, size = 10)
print(x)
Reverse(x)
rev(x)

## Code for 2

MergeSort <- function(x, y) {
  i = 1; j = 1; k = 1;
  z <- rep(0, length(x) + length(y))
  while (k <= length(x) + length(y)) {
    if (i <= length(x) & j <= length(y)) {
      if(x[i] >= y[j]) {
        z[k] <- y[j]
        j <- j + 1
        k <- k + 1
      }
      else {
        z[k] <- x[i]
        i <- i + 1
        k <- k + 1
      }
    }
    else if (i <= length(x) & j > length(y)) {
      z[k] <- x[i]
      i <- i + 1
      k <- k + 1
      }
    else if (i > length(x) & j <= length(y)) {
      z[k] <- y[j]
      j <- j + 1
      k <- k + 1
      }
    }
  return(z)
}

## Test for 2

x <- runif(10)
x <- sort(x)

y <- runif(10)
y <- sort(y)

MergeSort(x, y)

## Code for 3

ZipperSort <- function(x, y) {
  i = 1; j = 1; k = 1;
  z <- rep(0, length(x) + length(y))
  while (k <= length(x) + length(y)) {
    if (i <= length(x) & j <= length(y)) {
      if(i <= j) {
        z[k] <- x[i]
        i <- i + 1
        k <- k + 1
      }
      else {
        z[k] <- y[j]
        j <- j + 1
        k <- k + 1
      }
    }
    else if (i <= length(x) & j > length(y)) {
      z[k] <- x[i]
      i <- i + 1
      k <- k + 1
    }
    else if (i > length(x) & j <= length(y)) {
      z[k] <- y[j]
      j <- j + 1
      k <- k + 1
    }
  }
  return(z)
}

## Test for 3

x <- c(1, 2, 3)
y <- c(4, 5, 6, 7, 8)

ZipperSort(x, y)

## Code for 4

nSum <- function(x) {
  i = 1;
  sum = 0
  while (i <= x) {
    if (i %% 3 == 0) {
      sum = sum + i
      i <- i + 1
    }
    else if (i %% 5 == 0) {
      sum = sum + i
      i <- i + 1
    }
    else {
      i <- i + 1
    }
  }
  return(sum)
}

## Test for 4

nSum(20)
nSum(50)
nSum(100)

##Code for 5

CommonDivisors <- function(x, y) {
  z <- c(1)
  i <- 2
  while(i <= x & i <= y) {
    if (x %% i == 0 & y %% i == 0) {
      z <- append(z, i)
      i <- i + 1
    }
    else {
      i <- i + 1
    }
  }
  if (length(z) != 1) {
    return(cat("The common factors of", x, "and", y, "are", z))
  }
  else {
    return(cat(x, "and", y, "are relatively prime."))
  }
}

## Test for 5

CommonDivisors(125, 169)
CommonDivisors(120, 160)

## Code for 6

DigitFinder <- function(x) {
  z <- c()
  while(x > 0) {
    z <- append(z, x %% 10)
    x <- floor(x / 10)
  }
  z <- Reverse(z)
  return(z)
}

## Test for 6

DigitFinder(325)
DigitFinder(123456)

## Code for 7

Palindrome <- function(x) {
  read <- x
  while (x <= 1000000) {
    if(identical(DigitFinder(x), Reverse(DigitFinder(x)))) {
      return(cat(x, "is the closet palindrome greater than", read))
    }
    else {
      x <- x + 1
    }
  }
}


## Test for 7

Palindrome(45678)
Palindrome(9876)

## Code for 9

## Building a Graph

rock <- rock ## Load in built-in Data set 'Rock'
attach(rock)

## Line plot
plot(peri, area, type = "p", col = 'red', 
     xlab = "Perimeter", ylab = "Area")

## Boxplot

boxplot(area~perm, col = rainbow(12),
        xlab = "Perm", ylab = "Area")

## Bar Chart
barplot(shape, col = rainbow(length(shape)))

##Pie Chart

rock_labels <- c(6.3, 17.1, 119, 82.4, 58.6, 
                 142, 740, 890, 950, 100, 1300, 580)

rock_labels2 <- round(rock_labels/sum(rock_labels) * 100, digits = 1)

rock_labels2 <- paste(rock_labels2, "%", sep = "")

pie(rock_labels, main = "Perm %", col = rainbow(length(rock_labels)), 
    labels = rock_labels2)