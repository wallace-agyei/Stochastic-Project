setwd("C:/Users/wallace/Desktop/StochasticLab2")
#install.packages("survminer")


library(tidyverse)
library(readxl)
library(dplyr)
library(survminer)
library(survival)
library(ggplot2)

# Question(a)

data<- read.delim("Thoracic.txt", sep = " ")
colnames(data) <- c("DGN", "PRE4", "PRE5", "PRE6", "PRE7", "PRE8", "PRE9", "PRE10", "PRE11", "PRE14", "PRE17", "PRE19", "PRE25", "PRE30", "PRE32", "AGE", "Risky1Y") 
survival<- data[c("PRE30", "AGE", "Risky1Y")]
survival


#Non-Parametric Estimations of the Survival Functions
#Kaplan Meier
fit.km <- surv_fit(Surv(AGE,Risky1Y)~1,data=survival, type='kaplan-meier')
summary(fit.km)

#Fleming-Harrington
fit.fh <- surv_fit(Surv(AGE,Risky1Y)~1,data=survival, type='fleming-harrington')
summary(fit.fh)

#Plot of Kaplan Meier and Fleming-Harrington
plot1 <- plot(fit.km, col = "red", ylab ="S(t)", xlab="t")
lines(fit.fh, col='blue')
legend( par("usr")[2], par("usr")[4], yjust=3, xjust=1.4,
        c("Kaplan-Meier", "Fleming-Harrington"),
        lwd=c(1,1), lty=c(1,1),
        col=c("red", 'blue'))

plot1


#fitting exponential model to the data
fit.exp <- survreg(Surv(AGE,Risky1Y) ~ 1, data = survival, dist="exponential")
exp.lambda <- exp(-fit.exp$coefficients)
exp.estim<- exp(-exp.lambda*c(0:90))

#fit the Weibull model to the data
fit.weib <- survreg(Surv(AGE,Risky1Y) ~ 1, data = survival, dist="weibull")
weib.lambda <- exp(-fit.weib$coefficients)
alpha = 1/fit.weib$scale
weib.estim<- exp(-(weib.lambda*c(0:90))^alpha)

#Plot of Exponential,Kaplan Meier,Weibull model into data
plot2 <- plot(fit.km, col = "blue", ylab ="Survival", xlab="time")
lines(exp.estim, col = "red")
lines(weib.estim, col = "green")
legend("bottomleft", inset=0.05,
       legend =  c("Kaplan-Meier", "Exponential", "Weibull"),
       col =  c("blue", "red", "green"),
       lwd=c(2,1), lty=c(2,1),
       box.lty=0)

plot2

# Is Weibull model is adequate for the data?

df <- data.frame(x=log(fit.km$time) ,y=log(-log(fit.km$surv)))


plot3 <- ggplot(df) +
  geom_point(aes(x,y), color = 'navy') +
  geom_abline(aes(intercept = alpha*log(weib.lambda), slope = alpha, color = 'Line'))+
  scale_colour_manual(name = " ", values = c("red")) +
  xlab('time') +
  ylab("s(t)") +
  theme_classic(base_size = 10) + 
  theme(legend.position = c(0.2, 0.95),legend.justification = c("right", "top"), legend.key = element_rect(fill = "white", colour = "gray19"))
plot3


#Question(b)

#Splitting the data into two groups of Smokers and Non-Smokers

smoker <- subset(survival, PRE30 == 'TRUE')
non.smoker <- subset(survival, PRE30 =='FALSE')

#Proportion of smokers in the sample
propt <- nrow(smoker)/nrow(survival)*100 #in percetage
propt

#Kaplan-Meier estimators 
km.smoker.fit <- survfit(Surv(AGE, Risky1Y) ~ PRE30, type="kaplan-meier",data = survival)
ggsurvplot(km.smoker.fit)+
  labs(x = "t", y = "S(t)") +
  guides(fill=FALSE)


#Smoker Weibull
weib.smoker <- survreg(Surv(AGE,Risky1Y) ~ 1, data = smoker, dist="weibull")
weib.smoker.lambda <- exp(-weib.smoker$coefficients)
alpha = 1/weib.smoker$scale
weib.smoker.estim<- exp(-(weib.smoker.lambda*c(0:90))^alpha)

#Non-Smoker Weibull

weib.non.smoker <- survreg(Surv(AGE,Risky1Y) ~ 1, data = non.smoker, dist="weibull")
weib.non.smoker.lambda <- exp(-weib.non.smoker$coefficients)
alpha = 1/weib.non.smoker$scale
weib.non.smoker.estim<- exp(-(weib.non.smoker.lambda*c(0:90))^alpha)

plot3 <- plot(weib.smoker.estim, col = "blue", type = "l", ylab ="S(t)", xlab="t")
lines(weib.non.smoker.estim, col='red')
legend("bottomleft", inset=.0001,
       legend =  c("smokers", "non-smokers"),
       col =  c("blue", "red"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
plot3


# Survival Functions: Kaplan-Meier
km.smoker<- survfit(Surv(AGE, Risky1Y) ~ 1, conf.int = 0.95, data = smoker, type = 'kaplan-meier')
km.non.smoker <-  survfit(Surv(AGE, Risky1Y) ~ 1, conf.int = 0.95, data = non.smoker, type = 'kaplan-meier')



plot4<- plot(weib.smoker.estim, col = "red", type = "o", ylab ="S(t)", xlab="t")
lines(weib.non.smoker,estim, col='blue',type='l')
lines(km.smoker, col = "green")
lines(km.non.smoker, col = "black")
legend("bottomleft", inset=.0001,
       legend =  c("Weibull smokers", "Weibull nonsmokers", "KM-smokers", "KM-non.smokers"),
       col =  c("red", "blue", "green", "black"),
       lwd=c(1,1), lty=c(1,1),
       box.lty=0)
plot4
