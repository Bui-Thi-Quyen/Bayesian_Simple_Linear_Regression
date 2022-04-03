# Dau vao
mu<-5
tau <- 0.25
# Mo phong
########################################################
#Sample size n=10
n<-10
x<-rnorm(n, mean = mu, sd=1/sqrt(tau))
# MLE
x.bar<-sum(x)/n
x1 <- matrix(x,nrow = 1)
x1<- (x1 - x.bar)^2
S2 <- 1/n * sum(x1)
############################
# 1. Tien nghiem lien hop
# Prior information
#Scenario 1.1.1: underguest, low confidence (du doan duoi, tu tin th???p)
g1<-0.3
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)

#Scenario 1.1.1: underguest, low confidence (du doan duoi, tu tin trung binh)
g1<-0.3
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-4.9999
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)


#Scenario 1.1.1: underguest, low confidence (du doan duoi, tu tin trung binh)
g1<-0.3
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-6
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)

#Scenario 1.1.1: underguest, medium confidence (du doan duoi, tu tin trung binh)
g1<-0.3
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)

#Scenario 1.1.1: underguest, medium confidence (du doan duoi, tu tin trung binh)
g1<-0.3
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-4.999
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)

#Scenario 1.1.1: underguest, medium confidence (du doan duoi, tu tin trung binh)
g1<-0.3
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-6
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)

#Scenario 1.1.1: underguest, medium confidence (du doan duoi, tu tin trung binh)
g1<-0.3
rho<-0.3
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-6
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)
############################
#Scenario 1.1.2: underguest, low confidence (du doan duoi, tu tin thap)
g1<-0.1
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
c(mu_MSE,mu_MLE)
#Scenario 1.1.3: underguest, high confidence (du doan duoi, tu tin cao)
g1<-0.1
rho<-0.3
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
c(mu_MSE,mu_MLE)
#Scenario 1.2.1: precise guest, low confidence (du doan chinh xac, tu tin thap)
g1<-0.25
rho<-0.9
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)
#Scenario 1.2.2: precise guest, medium confidence (du doan chinh xac, tu tin trung binh)
g1<-0.25
rho<-0.6
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)
#Scenario 1.2.3: precise guest, high confidence (du doan chinh xac, tu tin cao)
g1<-0.25
rho<-0.3
g2<-g1*rho
# Prior elicitation trich xuat gia tri sieu tham so cho tau
a0<-(g1/g2)^2
c0<-g1/g2^2
# Prior elicitation trich xuat gia tri sieu tham so cho Mu
g3<-2
g4<-g3*rho
m0<-g3
b0<- c0/(g4^2*(a0-1))
k<- b0/(b0+n)
a1<- a0+n/2
c1 <- c0+n/2*(S2+k*(x.bar-m0)^2)
m1<-(1-k)*x.bar + k*m0
b1 <- b0 +n
# Estimate
mu_MSE <- m1
mu_MLE <-x.bar
tau_MSE <- a1/c1
tau_MLE <- 1/S2
c(mu_MSE,mu_MLE)
c(tau_MSE,tau_MLE)
#Scenario 1.2.3: precise guest, high confidence (du doan chinh xac, tu tin cao)
