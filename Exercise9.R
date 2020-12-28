setwd("C:/Users/wallace/Desktop/StochasticLab2")
install.packages("fitdistrplus")

library(tidyverse)
library(bootstrap)
library(fitdistrplus)
set.seed(42)

n <- 100 #the sample size 
R <- 1000 #the number of bootstrap replications
M <- 1000 #the number of Monte Carlo samples 

#Question(a)
lambda=13
k=1
var<-(lambda^2)*(gamma(1+2/k)-(gamma(1+1/k))^2)
sigma<-var^0.5
sigma
xmed<-lambda*(log(2))^(1/k)
xmed

#two-sided bootstrap percentile confidence interval
succes.median <- 0
succes.sd <- 0
alpha <- 0.95
med.lower <- 0  #lower-bound of median
med.upper <- 0 #upper-bound of median
sd.lower <- 0  #lower-bound of standard deviation
sd.upper <- 0  #upper-bound of standard deviation

for (j in 1:M) {
  sample_weibull <- rweibull(n, k, lambda)
  bootmedian <- 0
  bootsd <- 0
  for (i in 1:R) {
    bootstrap <- sample(sample_weibull, n, replace = T)
    bootmedian[i] <- median(bootstrap)
    bootsd[i] <- sd(bootstrap)
  }
  


bootmedian <- sort(bootmedian)
bootsd <- sort(bootsd)


med.lower[j] <- bootmedian[floor(R*(1-alpha))/2]
med.upper[j] <- bootmedian[floor(R*(1-(1-alpha)/2))] 
sd.lower[j] <- bootsd[floor(R*(1-alpha))/2] 
sd.upper[j] <- bootsd[floor(R*(1-(1-alpha)/2))] 
df1 <- data.frame(med.lower, med.upper, sd.lower, sd.upper)
}
df1 <- df1 %>% 
  mutate(succes.median = xmed >= med.lower & xmed <= med.upper)
df1 <- df1 %>% 
  mutate(succes.sd = sigma >= sd.lower & sigma <= sd.upper)


#Probability coverage
prob.cov1<- c(sum(df1$succes.median)/M, sum(df1$succes.sd)/M)

#Average interval length
avg.length1 <- c(sum(df1$med.upper - df1$med.lower)/nrow(df1), sum(df1$sd.upper - df1$sd.lower)/nrow(df1))


n <- 1000
R <- 1000
succes.median <- 0
sucess.sd <- 0
alpha <- 0.95
med.lower <- 0
med.upper <- 0
sd.lower <- 0
sd.upper <- 0

for (j in 1:M) {
  sample_weibull <- rweibull(n, k, lambda)
  bootmedian <- 0
  bootsd <- 0
  for (i in 1:R) {
    bootstrap <- sample(sample_weibull, n, replace = T)
    bootmedian[i] <- median(bootstrap)
    bootsd[i] <- sd(bootstrap)
  }


bootmedian <- sort(bootmedian)
bootsd <- sort(bootsd)

med.lower[j] <- bootmedian[floor(R*(1-alpha))/2]
med.upper[j] <- bootmedian[floor(R*(1-(1-alpha)/2))] 
sd.lower[j] <- bootsd[floor(R*(1-alpha))/2] 
sd.upper[j] <- bootsd[floor(R*(1-(1-alpha)/2))] 
df2 <- data.frame(med.lower, med.upper, sd.lower, sd.upper)
}
df2 <- df2 %>% 
  mutate(succes.median = xmed >= med.lower & xmed <= med.upper)
df2 <- df2 %>% 
  mutate(succes.sd = sigma >= sd.lower & sigma <= sd.upper)


#Probability coverage

prob.cov2<- c(sum(df2$succes.median)/M, sum(df2$succes.sd)/M)

#Average length

avg.length2 <- c(sum(df2$med.upper - df2$med.lower)/nrow(df2), sum(df2$sd.upper - df2$sd.lower)/nrow(df2))


n <- 100
R <- 5000
succes.median <- 0
sucess.sd <- 0
alpha <- 0.95
med.lower <- 0
med.upper <- 0
sd.lower <- 0
sd.upper <- 0

for (j in 1:M) {
  sample_weibull <- rweibull(n, k, lambda)
  bootmedian <- 0
  bootsd <- 0
  for (i in 1:R) {
    bootstrap <- sample(sample_weibull, n, replace = T)
    bootmedian[i] <- median(bootstrap)
    bootsd[i] <- sd(bootstrap)
  }
  
  bootmedian <- sort(bootmedian)
  bootsd <- sort(bootsd)
  
  med.lower[j] <- bootmedian[floor(R*(1-alpha))/2]
  med.upper[j] <- bootmedian[floor(R*(1-(1-alpha)/2))] 
  sd.lower[j] <- bootsd[floor(R*(1-alpha))/2] 
  sd.upper[j] <- bootsd[floor(R*(1-(1-alpha)/2))] 
  df3 <- data.frame(med.lower, med.upper, sd.lower, sd.upper)
}
df3 <- df3 %>% 
  mutate(succes.median = xmed >= med.lower & xmed <= med.upper)
df3 <- df3 %>% 
  mutate(succes.sd = sigma >= sd.lower & sigma <= sd.upper)


#Probability coverage

prob.cov3<- c(sum(df3$succes.median)/M, sum(df3$succes.sd)/M)

#Average length

avg.length3 <- c(sum(df3$med.upper - df3$med.lower)/nrow(df3), sum(df3$sd.upper - df3$sd.lower)/nrow(df3))

#bootstrap accelerated bias-corrected CI
# for median

med.lower <- rep(0, M)
med.upper <- rep(0, M)
zmed <- 0
acc.med <- 0 #
for (j in 1:M) {
  sample_weibull <- rweibull(100, k, lambda)
  bcan<- bcanon(sample_weibull, R, theta=median, alpha = c(0.025, 0.975))
  zmed[j] <- bcan$z0
  acc.med[j] <- bcan$acc
  med.lower[j] <- bcan$confpoints[1,2]
  med.upper[j] <- bcan$confpoints[2,2]
}

#We create a dataframe of the bcanon generated CIs with the confpoints parameters
df.bcan.med<- data.frame(med.lower, med.upper)

df.bcan.med <- df.bcan.med %>% 
  mutate(succes.median = xmed >= med.lower & xmed <= med.upper)



#Then with the sd
sd.lower <- 0
sd.upper <- 0
zsd <- 0
acc.sd <- 0
theta = sd
for (j in 1:M) {
  sample_weibull <- rweibull(100, k, lambda)
  bcan <-  bcanon(sample_weibull, R, theta = median, alpha = c(0.025, 0.975))
  zsd[j] <- bcan$z0
  acc.sd[j] <- bcan$acc
  sd.lower[j] <- bcan$confpoints[1,2]
  sd.upper[j] <- bcan$confpoints[2,2]
}

df.bcan.sd <- data.frame(sd.lower, sd.upper)

df.bcan.sd <- df.bcan.sd%>% 
  mutate(succes.sd = sigma >= sd.lower & sigma <= sd.upper)


result <-  data.frame(zsd, acc.sd, zmed, acc.med) 
df4 <- data.frame(df.bcan.med, df.bcan.sd) 

#Probability coverage
prob.cov4<- c(sum(df4$succes.median)/M, sum(df4$succes.sd)/M)

#Estimation of the average interval length
avg.length4<- c(sum(df4$med.upper - df4$med.lower)/nrow(df4), sum(df4$sd.upper - df4$sd.lower)/nrow(df4))




#Question(b)
data<- read.delim("shhs2.txt")
view(data)

plot1<-ggplot(data,aes(x=rdi4p))+ 
  geom_histogram(color="blue",alpha=0.5,bins=20)+xlab("x")+
  ylab("Frequency")+ggtitle("Histogram")+
  theme(plot.title = element_text(hjust = 0.5))
plot1



fit.data<-fitdist(data$rdi4p,"weibull",method="mle",lower = c(0, 0) ,start = list(scale = 1, shape = 1))
scale.data<-fit.data$estimate[1]
shape.data<-fit.data$estimate[2]

#plot weibull
plot2<-ggplot(data,aes(x=rdi4p))+ geom_density(color="blue",alpha=0.5)+
  stat_function(fun=dweibull,args=list(shape=shape.data,scale=scale.data),color="red")+
  xlab("x")+ylab("Density")+ggtitle("Density")+
  theme(plot.title = element_text(hjust = 0.5))
plot2


#bootstrap percentile 
rdi4p.med <- median(data$rdi4p)
rdi4p.sigma <- sd(data$rdi4p)
alpha <- 0.95
n <- length(data$rdi4p)
R <- 1000 

med <- 0
sd <- 0
for (i in 1:R) {
  bootstrap <- sample(data$rdi4p, n, replace = T)
  med[i] <- median(bootstrap)
  sd[i] <- sd(bootstrap)
}
med<- sort(med)
sd<- sort(sd)
med.lower <- med[floor(R*(1-alpha))/2]
med.upper <- med[floor(R*(1-(1-alpha)/2))]
sd.lower<- sd[floor(R*(1-alpha))/2]
sd.upper <- sd[floor(R*(1-(1-alpha)/2))]


boostrap.percentile<- data.frame(med.lower, med.upper, sd.lower, sd.upper)
succes.median <- rdi4p.med >= med.lower & rdi4p.med <= med.upper
succes.sd <- rdi4p.sigma >= sd.lower & rdi4p.sigma <= sd.upper

#bootstrap accelerated bias-corrected confidence intervals 
#from the data all the median values

rdi4p.remove <- data$rdi4p [! data$rdi4p %in% median(data$rdi4p)] 
bcan<- bcanon(rdi4p.remove, R, theta = median, alpha = c(0.025, 0.975))
zmed <- bcan$z0
acc.med <- bcan$acc
med.lower <- bcan$confpoints[1,2]
med.upper<- bcan$confpoints[2,2]
#median CI
succes.median.acc = rdi4p.med >= med.lower & rdi4p.med <= med.upper

#sd
bcan <- bcanon(data$rdi4p, R, theta = sd, alpha = c(0.025, 0.975))
zsd <- bcan$z0
acc.sd<- bcan$acc
med.lower <- bcan$confpoints[1,2]
med.upper <- bcan$confpoints[2,2]

succes.sd.acc = rdi4p.sigma >= sd.lower & rdi4p.sigma <= sd.upper



bcan.acc <- data.frame(med.lower, med.upper, sd.lower, sd.upper, zmed, acc.med, zsd, acc.sd)



