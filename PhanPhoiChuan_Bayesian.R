# Dau vao
mu<-5
sigma<-2
# Mo phong
n<-10
x<-rnorm(n, mean = mu, sd=sigma)
# MLE
s<-sum(x)
x.bar<-s/n
mu.MLE<-x.bar
mu.MLE
############################
# 1. Tien nghiem lien hop
# Prior information
#Scenario 1.1.1: underguest, low confidence (du doan duoi, tu tin th???p)
g1<-4
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)

#Scenario 1.1.2: underguest, medium confidence (du doan duoi, tu tin trung binh)
g1<-4
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)

#Scenario 1.1.3: underguest, high confidence (du doan duoi, tu tin cao)
g1<-4
rho<-0.3
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)

#Scenario 1.2.1: precise guest, low confidence (du doan chinh xac, tu tin th???p)
g1<-5
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)
#Scenario 1.2.2: precise guest, medium confidence (du doan duoi, tu tin trung bình)
g1<-5
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)
#Scenario 1.2.3: precise guest, high confidence (du doan duoi, tu tin cao)

g1<-5
rho<-0.3
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)
#Scenario 1.3.1: overguest, low confidence (du doan trên, tu tin th???p)

g1<-6
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)

#Scenario 1.3.2: overguest, medium confidence (du doan trên, tu tin trung bình)

g1<-6
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)
#Scenario 1.3.3: overguest, high confidence (du doan trên, tu tin cao)

g1<-6
rho<-0.3
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so
a0<-g1
b0<-g2
# Prior distribution
curve(dnorm(x, mean = a0, sd=b0), col="red", xlim = c(-1, 11), ylim=c(0, 2.5))
# Posterior distribution
k<-b0^2/(b0^2+sigma^2/n)
a1<-k*x.bar+(1-k)*a0
b1<-sigma*sqrt(k/n)
curve(dnorm(x, mean = a1, sd=b1), xlim = c(-1, 11), col="blue", add = T)
# Estimate
mu.MSE<-a1
c(mu.MLE, mu.MSE)
#############################################
############################
# 2. Tien nghiem deu
# Uniform prior [4, 6]
a<-4
b<-6
s<-sum(x)
x.bar<-s/n
ub<-pnorm(q=(b-x.bar)*sqrt(n)/sigma)
lb<-pnorm(q=(a-x.bar)*sqrt(n)/sigma)
K<-1/(ub-lb)
K
f<-function(x){exp(log(K)-n*(x-x.bar)^2/(2*sigma^2))}
curve(f(x), xlim = c(4, 8))
