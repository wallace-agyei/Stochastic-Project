setwd("C:/Users/wallace/Desktop/StochasticLab2")

library(ggplot2)
library(tidyverse)

set.seed(42)
student <- read.csv("student-mat.csv")

#Question(a)
#First we need to identify  the normality of each of G1, G2, G3


df1 <- data.frame(student$G1, rep('G1', nrow(student)))
colnames(df1) <- c("grades", "types")
df2 <- data.frame(student$G2, rep('G2', nrow(student)))
colnames(df2) <- c("grades", "types")
df3 <- data.frame(student$G3, rep('G3', nrow(student)))
colnames(df3) <- c("grades", "types")
df <- rbind(df1, df2, df3)

#Normal distributed(Histogram)

plot1<-ggplot(data=df,mapping = aes(x = grades))+ 
  geom_histogram(color="blue",alpha=0.5,bins=15)+xlab("grades")+ylab("Frequency")+
  facet_wrap(. ~types)
plot1

#Normal distributed(Density)
plot2<- ggplot(data = df, mapping = aes(sample = grades)) + 
  geom_density(aes(x = grades), fill = "lightblue") +
  facet_wrap(. ~types)
plot2


#Poisson distributed(Q-Q plots)
plot3<- ggplot(data = df, mapping = aes(sample = grades)) + 
  stat_qq(distribution = stats::qpois, dparams = list(lambda = mean(df$grades))) +
  geom_abline(alpha = 0.25) +
  facet_wrap(. ~types)
plot3


#Creating a dataframe to compute the variance-to-mean ratio for overdispersion check
table1 = data.frame(student$G1, student$G2, student$G3)
x <- apply(table1, 2, mean)
y <- apply(table1, 2, var)

var_to_mean_ratio <- y/x  #Checks overdispersion too

var_to_mean_ratio



# we implement the anscombe residual formua for Poisson distribution
anscombe.residuals <- function(y, mu){
  (3*(y**(2/3)-mu**(2/3)))/2*(mu**(1/6))
}

#Question(b)
model1 <- glm(formula = G1 ~. -G2 -G3, family = poisson, data = student) 
summary(model1)

#Goodness of Fit

pchisq(deviance(model1),df.residual(model1))

#Pearson residuals for model1

pearson.residual <- residuals(model1, "pearson")
par(mfrow=c(1,2))
hist(pearson.residual)# A histogram plot
qqnorm(pearson.residual)# A quantile normal plot - good for checking normality
qqline(pearson.residual)
#Anscombe residuals for model1
anscombe.residual<-  anscombe.residuals(student$G1, model1$fitted.values)
par(mfrow=c(1,2))
hist(anscombe.residual)# A histogram plot
qqnorm(anscombe.residual) # A quantile normal plot - good for checking normality
qqline(anscombe.residual)

#Residual analysis
  # Split the plotting panel into a 2 x 2 grid
plot(model1)

#Persue residual analysis
par(mfrow=c(2,2))
plot(model1)



#Question (c)
model2 <- glm(formula = G1 ~ sex + Fedu + studytime + failures + schoolsup + famsup + goout , family = poisson, data = student) 
summary(model2)

pchisq(deviance(model2),df.residual(model2))

#Analysis of deviance
anova(model2, model1, test = "Chisq")

#model 3 and comparison with model 2
model3 <- glm(formula = G1 ~ sex + Fedu + studytime + failures + schoolsup + famsup + Walc , family = poisson, data = student) 
summary(model3)





