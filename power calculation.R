#Pretest: calculate power

n <- 25
mu <- 107.5
sigma <- 15
mu0 <- 100

reps <- 9999

pvalues <- numeric(reps)

set.seed(20181210)

for (i in 1:reps){
  x <- rnorm(n, mu, sigma)
  t.stat <- (mean(x)-mu0)/(sd(x)/sqrt(n))
  pvalues[i] <- t.test(x, mu = mu0)$p.value
  
}

power <- mean(pvalues < 0.05)



#Test1: calculate power for two sample t test

n<-50
mean1 <- 25
mean2 <- 30
sigma <- 5

reps <- 10000
pvalues <- numeric(reps)

set.seed(20181210)

for (i in 1:reps){
  x1 <- rnorm(n, mean1, sigma)
  x2 <- rnorm(n, mean2, sigma)
  pvalues[i] <- t.test(x1,x2)$p.value
}

power <- mean(pvalues < 0.05)
power


#Test2: Calculate power for one sample proportion test using binomial exact test

prop.test(x = c(141,125), n = c(288,216)) # if the two sample proportion equals
prop.test(30,36,p = 0.5) #test if the proportion equals to null probability 0.5

reps <- 10000
set.seed(20181210)

## main part start here

for (j in 1:45) {
  
n <- j*10  
#n <- 50 + j*10 
#n <- 500

#suppose N = 500 and the true ORR is 0.5

# Create 10000 samples of binomial distributed random variables
heads <- rbinom(reps,size = n, prob = 0.5)

  for (i in 1:reps) {
  pvalues[i] <- prop.test(heads[i], n, p = 0.3)$p.value
  }

  power[j] <- mean(pvalues < 0.05)

}

which(power > 0.80)


