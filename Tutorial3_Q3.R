#################################### Q3 ##################################
setwd("E:/DoAnTotNghiep_Quyen")
data <- read.csv("DirectMarketing.csv", header = TRUE)
y1 <- data["AmountSpent"]
data[, c('AmountSpent', 'Salary', 'Children','Gender_b','Catalogs')]
y = matrix(unlist(y1), ncol = 1, byrow = TRUE)
n <- lengths(y1)
a <- matrix(rep(1,n),ncol = 1)
b <- matrix(unlist(data["Salary"]), ncol = 1, byrow = TRUE)
c <- matrix(unlist(data["Children"]), ncol = 1, byrow = TRUE)
d <- matrix(unlist(data["Gender_b"]), ncol = 1, byrow = TRUE)
e <- matrix(unlist(data["Catalogs"]), ncol = 1, byrow = TRUE)
X <- cbind(a,b,c,d,e)
X
dim(X)
dim <- dim(X)[2] # s??? h??? s??? = 5
A <- t(X)%*%X
Ainv<-solve(A)
Ainv
B <- t(X)%*%y
AmB <- Ainv%*%B
AmB
beta.MLE<-AmB[,1]
beta.MLE
sigma2.MLE<-t(y-X%*%beta.MLE)%*%(y-X%*%beta.MLE)/(n-dim)
sigma2.MLE

reg<-lm(y~b+c+d+e)
summary(reg)
N <- 25000
beta <- matrix(rep(0,N*dim),ncol = dim)
sig2 <- matrix(rep(0,N),ncol = 1)
sig2[1] <- 1
library(MASS)
beta[1,] <- mvrnorm(mu=AmB,Sigma = sig2[1]*Ainv)
for(i in 1:(N-1)){
  gamma_shape <- n/2
  a <- matrix(beta[i,],ncol=dim,byrow = TRUE)
  xb <- X%*%t(a)
  a2 <- y-xb
  gamma_rate <- t(a2)%*%(a2/2)
  auxilary <- rgamma(n = 1, shape = gamma_shape, rate = gamma_rate[1])
  sig2[i+1] <- 1/auxilary
  beta[i+1,] <- mvrnorm(mu=AmB, Sigma=sig2[i+1]*Ainv)
}
par(mfrow=c(2,3))
for(i in 1:dim){
  plot(beta[,i],type = "s", col = "blue")
}
plot(sig2,type = "s", col = "blue")

nburn <- 5000
beta <- beta[(nburn+1):N,]
sig2 <- sig2[(nburn+1):N,]
posterior_mean_beta1 <- mean(beta[, 1])
posterior_mean_beta1
posterior_mean_beta2 <- mean(beta[, 2])
posterior_mean_beta2
posterior_mean_beta3 <- mean(beta[, 3])
posterior_mean_beta3
posterior_mean_beta4 <- mean(beta[, 4])
posterior_mean_beta4
posterior_mean_beta5 <- mean(beta[, 5])
posterior_mean_beta5
beta.MSE<-c(posterior_mean_beta1, posterior_mean_beta2, 
            posterior_mean_beta3, posterior_mean_beta4, posterior_mean_beta5)
beta.MSE
beta.MLE
posterior_mean_sigma2 <- mean(sig2)
sigma2.MSE<-posterior_mean_sigma2
sigma2.MSE
posterior_std <- sqrt(mean(beta^2)-posterior_mean_beta1^2)
posterior_std
#det(sig2[i+1]*Ainv)
