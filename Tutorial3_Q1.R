# Bayesian computation
# Question 1
# part(i)
N.burnin <- 1000# Number of burnin
N.Iter <- N.burnin+5000# Number of iterations
n <- 50 #sample size
x.bar <- 5 #sample mean
# kernel of posterior distribution
k <- function(mu){1/(1+mu^2)*exp(n*x.bar*mu-n/2*mu^2)}
sigma = 0.0001# variance of epsilon
markov_chain<-rep(0, N.Iter) #create a vector to store the chain
mu0<-rnorm(1, mean = 0, sd = 1) # starting value
markov_chain[1] <-mu0 # first state of markov chain
i<-1
while(i< N.Iter){
  epsilon <- rnorm(1, mean = 0, sd = sigma)
  Y <- markov_chain[i] + epsilon
  alpha <- min(k(Y)/k(markov_chain[i]),1)
  u <- runif(1)
  if(u < alpha){markov_chain[i+1] <-Y}
  else{markov_chain[i+1] <- markov_chain[i]}
  i<-i+1
}
plot(markov_chain1,type = "s", col = "blue")
markov_chain
markov_chain1 = markov_chain[(N.burnin+1):N.Iter]
hist(markov_chain1, probability = T, xlim = c(4, 6), breaks = 1000)
lines(density(markov_chain), col="red", lwd=3)
posterior_mean_estimate <-mean(markov_chain1)
posterior_mean_estimate
posterior_variance_estimate <- mean((markov_chain - posterior_mean_estimate)^2)
posterior_variance_estimate
########################################
# Simulation study for normal distribution
# True parameter
mu<-5
# sample size
n <- 100
# Observations
x<-rnorm(n, mean=mu, sd=1)
s<-sum(x)
x.bar <- s/n
###############
N.burnin <- 1000# Number of iterations
N.Iter <- N.burnin+5000# Number of iterations
# kernel of posterior distribution
k <- function(mu){1/(1+mu^2)*exp(n*x.bar*mu-n/2*mu^2)}
sigma = 10^(-1)# variance of epsilon
markov_chain<-rep(0, N.Iter) #create a vector to store the chain
mu0<-rnorm(1, mean = 0, sd = 1) # starting value
markov_chain[1] <-mu0 # first state of markov chain
i<-1
while(i< N.Iter){
  epsilon <- rnorm(1, mean = 0, sd = sigma)
  Y <- markov_chain[i] + epsilon
  alpha <- min(k(Y)/k(markov_chain[i]),1)
  u <- runif(1)
  if(u < alpha){markov_chain[i+1] <-Y}
  else{markov_chain[i+1] <- markov_chain[i]}
  i<-i+1
}
plot(markov_chain,type = "s", col = "blue")
markov_chain
markov_chain1 = markov_chain[(N.burnin+1):N.Iter]
hist(markov_chain1, probability = T, xlim = c(4, 6), breaks = 1000)
lines(density(markov_chain), col="red", lwd=3)
posterior_mean_estimate <-mean(markov_chain1)
posterior_mean_estimate
posterior_variance_estimate <- mean((markov_chain - posterior_mean_estimate)^2)
posterior_variance_estimate
#############################
# Part (ii)
# Function with input being sigma
mu.MCMC<-function(sigma1)
{
  # number of iterations
  N <- 20000
  # kernel of posterior distribution
  k <- function(mu){1/(1+mu^2)*exp(n*x.bar*mu-n/2*mu^2)}
  # create a vector to store the chain
  mc<-rep(0, N)
  # gia tri ngau nhien ban dau theo N(0,1)
  mu0<-rnorm(1, mean = 0, sd = 1)
  mc[1] <-mu0
  i<-1
  while(i< N){
    e <- rnorm(1, mean = 0, sd = sigma1)
    Y <- mc[i] + e
    f<-(1+Y^2)/(1+mc[i]^2)*exp(n*x.bar*(Y-mc[i])-n/2*(Y^2-mc[i]^2))
    alpha <- min(f,1)
    u <- runif(1)
    if(u < alpha){mc[i+1] <-Y}
    else{mc[i+1] <- mc[i]}
    i<-i+1
  }
  mu.MCMC<-mean(mc)#posterior_mean_estimate 
  mu.MCMC
}
####################
# sigma=0.1
mu.hat<-rep(0, 10)
for (i in 1:10) {
  mu.hat[i]<-mu.MCMC(0.1)
}
MC.se1<-sd(mu.hat)
MC.se1
##############################
# part (iii)
# sigma=0.0001: markov chain is divergent
mu.hat<-rep(0, 10)
for (i in 1:10) {
  mu.hat[i]<-mu.MCMC(0.0001)
}
MC.se2<-sd(mu.hat)
MC.se2
# part (iv)
# sigma=10000: markov chain is divergent
mu.hat<-rep(0, 10)
for (i in 1:10) { 
  mu.hat[i]<-mu.MCMC(10000)
}
MC.se3<-sd(mu.hat)
MC.se3
# Conclusion:
c(MC.se1, MC.se2, MC.se3)

