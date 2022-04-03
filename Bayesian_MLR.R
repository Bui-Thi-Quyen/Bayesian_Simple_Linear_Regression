# Simulation study for multiple linear regression (MLR)
# Input
beta<- c(1, 2, 3, 4, 5)# he so hoi quy thuc te
sigma<-0.5#do lech chuan cua sai so
# Generate (X, Y)
n<- 100# co mau
k<-4# so bien doc lap
D<- diag(k)# ma tran don vi cap k
x<-matrix(rep(0, n*k), ncol = k) #ma tran n dong k cot
y<- rep(0, n)#khoi tao vector y
for (i in 1:n) {
  x[i,]<- mvrnorm(1, mu = c(1, 2, 3, 4), Sigma = D)# sinh bnn phan phoi chuan nhieu chieu
  mu<-c(1, x[i,])%*%beta# trung binh cua y|x
  y[i]<- rnorm(1, mean = mu, sd = sigma)  
}
head(x)
head(y)
d<-data.frame(y, x)
pairs(d)
cor(d)
# lm() function
reg<- lm(d$y~d$X1+d$X2+d$X3+d$X4)
summary(reg)
###########################
# MLE
x0<-rep(1, n)
x<-cbind(x0, x)
k<-ncol(x)
A<-solve(t(x)%*%x)
beta.MLE<- A%*%t(x)%*%y
sigma2.MLE<-t(y-x%*%beta.MLE)%*%(y-x%*%beta.MLE)/(n-1)
sigma.MLE<- sqrt(sigma2.MLE)
beta.MLE
sigma.MLE
##############################
# MSE with flat prior
N<- 5000
sigma2.mc1<- rep(0, N)
beta.mc1<- matrix(rep(0, N*k), ncol = k)
sigma2.mc1[1]<- 1
beta.mc1[1, ]<- rep(1, k)
for (i in 1:(N-1)) {
sigma2.mc1[i+1]<-1/rgamma(1, shape = n/2, rate = t(y-x%*%beta.mc1[i,])%*%(y-x%*%beta.mc1[i,])/2)
require(MASS)
beta.mc1[i+1,]<-mvrnorm(mu=beta.MLE, Sigma = sigma2.mc1[i+1]*solve(t(x)%*%x))
}
head(sigma2.mc1)
tail(sigma2.mc1)
head(beta.mc1)
tail(beta.mc1)
par(mfrow=c(2, 3))
plot(sigma2.mc1, type = "s", col = "blue")
plot(beta.mc1[, 1], type = "s", col = "blue")
plot(beta.mc1[, 2], type = "s", col = "blue")
plot(beta.mc1[, 3], type = "s", col = "blue")
plot(beta.mc1[, 4], type = "s", col = "blue")
plot(beta.mc1[, 5], type = "s", col = "blue")
posterior_mean_beta1 <- mean(beta.mc1[, 1])
posterior_mean_beta2 <- mean(beta.mc1[, 2])
posterior_mean_beta3 <- mean(beta.mc1[, 3])
posterior_mean_beta4 <- mean(beta.mc1[, 4])
posterior_mean_beta5 <- mean(beta.mc1[, 5])
beta.mc.FLAT<-c(posterior_mean_beta1, posterior_mean_beta2, 
            posterior_mean_beta3, posterior_mean_beta4, posterior_mean_beta5)
############################################
# Conjugate prior
N<- 5000
sigma2.mc2<- rep(0, N)
beta.mc2<- matrix(rep(0, N*k), ncol = k)
sigma2.mc2[1]<- 1
beta.mc2[1, ]<- rep(1, k)
a0<-2
b0<-1
MU0<- c(0.5, 1.5, 2.5, 3.5, 4.5)
SIGMA0<-diag(k)
for (i in 1:(N-1)) {
  a1<-a0+n/2
  b1<-b0+t(y-x%*%beta.mc2[i,])%*%(y-x%*%beta.mc2[i,])/2
  sigma2.mc2[i+1]<-1/rgamma(1, shape = a1, rate = b1)
  SIGMA.y<-sigma2.mc2[i+1]*A
  MU.y<-A%*%t(x)%*%y
  SIGMA1<-solve(solve(SIGMA0)+solve(SIGMA.y))
  MU1<-SIGMA1%*%solve(SIGMA0)%*%MU0+SIGMA1%*%solve(SIGMA.y)%*%MU.y
  require(MASS)
  beta.mc2[i+1,]<-mvrnorm(mu=MU1, Sigma = SIGMA1)
}
head(sigma2.mc2)
tail(sigma2.mc2)
head(beta.mc2)
tail(beta.mc2)
par(mfrow=c(2, 3))
plot(sigma2.mc2, type = "s", col = "blue")
plot(beta.mc2[, 1], type = "s", col = "blue")
plot(beta.mc2[, 2], type = "s", col = "blue")
plot(beta.mc2[, 3], type = "s", col = "blue")
plot(beta.mc2[, 4], type = "s", col = "blue")
plot(beta.mc2[, 5], type = "s", col = "blue")

posterior_mean_beta1 <- mean(beta.mc2[, 1])
posterior_mean_beta2 <- mean(beta.mc2[, 2])
posterior_mean_beta3 <- mean(beta.mc2[, 3])
posterior_mean_beta4 <- mean(beta.mc2[, 4])
posterior_mean_beta5 <- mean(beta.mc2[, 5])
beta.MSE<-c(posterior_mean_beta1, posterior_mean_beta2, 
                posterior_mean_beta3, posterior_mean_beta4, posterior_mean_beta5)

