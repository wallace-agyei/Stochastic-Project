setwd("C:/Users/wallace/Desktop/StochasticLab2")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Question (a)
#ASimulate N =1000 binomial random variables B(n = 10; p = 0:4)
#Swtich radom number generator to Wichmann-Hill 

RNGkind()
RNGkind(kind="Wichmann-Hill")
RNGkind()

#Inversion method
N = 1000
n = 10
p = 0.4
uniform<- runif(N)
bins <- .bincode(uniform, breaks = c(0, pbinom(0:10, 10, 0.4)), right = F, include.lowest = T)

inv.bins <- numeric()
for(i in 1:N){
  inv.bins[i] <- bins[i]-1
}
#Simulating N=1000 obs by sum of indep. bernoulli
bin.berno <- numeric()
for (i in 1:N){
  v <- runif(n)
  bin.berno[i] <- sum(v < p)
}

#Simulating binomial with R function rbinom
bin.binom = rbinom(N, n, p)

datasim<-data.frame(x=c(inv.bins,bin.berno,bin.binom),Method=c("inversion","bernoulli","rbinom"),each=1000)

plot1<-ggplot(datasim, aes(x = x,fill=Method)) + geom_density(alpha = 0.5)+ggtitle("Density by Method")+
  theme(plot.title = element_text(hjust = 0.5))
plot1

#Switching the random number generator back to its default
RNGkind(kind = "default", normal.kind = NULL)

#Question(b)
#Simulate N=10 000 f(x)=Stad Norm using acceptance/rejection method
eqn = function(x){sqrt(pi/2)*exp(-x^2/2)*(1+x^2)}
plot2<-ggplot(data.frame(x=c(-5, 5)), aes(x=x))+ stat_function(fun=eqn, geom="line") + xlab("x") + ylab("h(x)")+ggtitle("h(x) plot")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+geom_vline(xintercept = c(-1,0,1),color = "red")
plot2


f <- function(x){
  ((2*pi)^(-1/2))*exp(-(x^2)/2)
}

g <- function(x){
  (pi*(1 + x^2))^(-1)
}

#First determine the best value of the constant c, such that f(x) <= g(x)
x <- -100:100
c <- max(f(x)/g(x))


#generating Cauchy distribution with inversion method
N <- 10000
j <- 0
random.num <- numeric()
while(length(random.num) != N){
  w <- runif(1)  
  cauchy <- tan((w-(1/2))*pi) #cauchy distrib using inverse method
  U <- runif(1) 
  if(U*c*g(cauchy) <= f(cauchy)){ #acceptance - reject condition
    random.num[j] <- cauchy 
    j <- j + 1
  }
}

#Histogram of the obtained sample with the standard normal density
k <- rnorm(N)
df <- data.frame(random.num, k)
plot3 <- ggplot(df) +
  geom_histogram(aes( x = random.num, y = ..density.., colour = random.num), colour ="white") +
  geom_density(aes(x = k), colour = "red")
plot3

#QQ-plot
plot4 <- ggplot(data = df, mapping = aes(sample = random.num)) +
  stat_qq()+ stat_qq_line(color='red')+
  ggtitle("Normal QQ-Plot")
plot4

#The standard Cauchy density h(x)=g(x)/f(x) is unbounded therefore is not
#possible to simulate using the accept-reject method

eqn = function(x){(1/(pi*(1+x^2)))*((2*pi)^1/2)*exp(x^2/2)}
plot5<-ggplot(data.frame(x=c(-3, 3)), aes(x=x))+ stat_function(fun=eqn, geom="line") + xlab("x") + ylab("h(x)")+ggtitle("h(x) plot")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+geom_vline(xintercept = c(-1,0,1),color = "red")
plot5
