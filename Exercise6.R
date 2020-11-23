setwd("C:/Users/wallace/Desktop/StochasticLab2")

#install.packages("splines")
library(ggplot2)
library(splines)
library(readxl)
library(dplyr)
library(tidyverse)
library(splines)
library(nlme)

#Question(a)
#load data
stemcell <- read.table("stemcell.txt", col.names = "order")
head(stemcell)


sp.reg<- function(Y, X, knots, degree){
  # Y: response
  # X: covariate
  # knots: number of knots
  # degree: degree of the polynomial
  
  order<- degree + 1
  
  # knots
  knots = seq(min(X), max(X), length.out = knots+2)
  knots = c(min(X) - (order-1):1 , knots, max(X) + 1:(order-1)) 
  
  # B-splines
  N <- spline.des(knots, X, ord = order)$design
  
  # Coefficients
  coeff <- lm(Y ~ N - 1)$coefficients 
  
  # regression spline
  f.hat <- function(x){
    n <- spline.des(knots, x, ord = order,  outer.ok = TRUE)$design
    as.numeric(n %*% coeff)
  }
  return(f.hat)
}

#Fix the spline degree to 2
Y <- stemcell$order
X <- seq(10, 1440,10) 

# Estimated splines for different knots
fitk1 <- sp.reg(Y, X, 3, degree = 2)
fitk2 <- sp.reg(Y, X, 14, degree = 2)
fitk3 <- sp.reg(Y, X, 22, degree = 2)
fitk4 <- sp.reg(Y, X, 31, degree = 2)

# Plot all resulting fits on one plot
Time = seq(10, 1440, 10)
df1 <- data.frame(x = Time, y = fitk1(Time))
df2 <- data.frame(x = Time, y = fitk2(Time))
df3 <- data.frame(x = Time, y = fitk3(Time))
df4 <- data.frame(x = Time, y = fitk4(Time))

plot1<- ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df1, aes(color = "3")) +
  geom_line(data = df2, aes(color = "14")) +
  geom_line(data = df3, aes(color = "22")) +
  geom_line(data = df4, aes(color = "31")) +
  scale_colour_manual(name = "Knots", values = c("red", "blue", "yellow", "green")) +
  xlab("Time") + ylab("Parameter")

plot1

# Fix the number of knots to 4
fitd1 <- sp.reg(Y, X, 4, degree = 1)
fitd2 <- sp.reg(Y, X, 4, degree = 2)
fitd3 <- sp.reg(Y, X, 4, degree = 3)
fitd4 <- sp.reg(Y, X, 4, degree = 4)


#dataframes for the plot
df5 <- data.frame(x = Time, y = fitd1(Time))
df6 <- data.frame(x = Time, y = fitd2(Time))
df7 <- data.frame(x = Time, y = fitd3(Time))
df8 <- data.frame(x = Time, y = fitd4(Time))
#The plot
plot2<- ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df5, aes(color = "1")) +
  geom_line(data = df6, aes(color = "2")) +
  geom_line(data = df7, aes(color = "3")) +
  geom_line(data = df8, aes(color = "4")) +
  scale_colour_manual(name = "Degrees", values = c("red", "blue", "yellow", "green")) +
  ggtitle("Non parametric smooth lines")+
  xlab("Time") +ylab("Parameter")


plot2


#Question(b)

gcv_knots<- function(knots, degree){
  fitk<- sp.reg(Y, X, knots, degree)
  fitk<- fitk(X)
  
  a <- norm(Y - fitk, type = "2")
  n <- length(Y)
  
  residual <- (a**2)/(1 - knots/n)**2
  
  return(residual)
}
#Vectorize the GCV
GCV <- Vectorize(gcv_knots, vectorize.args = c("knots"))


max_knots <- 50

gcvk1<- which(GCV(1:max_knots, degree = 1) == min(GCV(1:max_knots, degree = 1)))
gcvk2<- which(GCV(1:max_knots, degree = 2) == min(GCV(1:max_knots, degree = 2)))
gcvk3<- which(GCV(1:max_knots, degree = 3) == min(GCV(1:max_knots, degree= 3)))
gcvk4<- which(GCV(1:max_knots, degree= 4) == min(GCV(1:max_knots, degree= 4)))

gcvk <- data.frame(degree1 = gcvk1, degree2 = gcvk2,degree3 = gcvk3, degree4 = gcvk4)

gcvk

# Calculate fits with GCV knots number, for degrees 1 to 4
fitk1 <- sp.reg(Y, X, knots = gcvk1, degree = 1)
fitk2 <- sp.reg(Y, X, knots= gcvk2, degree = 2)
fitk3 <- sp.reg(Y, X, knots= gcvk3, degree = 3)
fitk4 <- sp.reg(Y, X, knots= gcvk4, degree = 4)

# Plot resulting estimators on one plot
#Dataframes for ggplot
df9 <- data.frame(x = Time, y = fitk1(Time))
df10 <- data.frame(x = Time, y = fitk2(Time))
df11 <- data.frame(x = Time, y = fitk3(Time))
df12 <- data.frame(x = Time, y = fitk4(Time))

#plot
plot3<- ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df9, aes(color = "1")) +
  geom_line(data = df10, aes(color = "2")) +
  geom_line(data = df11, aes(color = "3")) +
  geom_line(data = df12, aes(color = "4")) +ggtitle("Non parametric smooth lines")+
  scale_colour_manual(name = "Poly.Degree",  values = c("blue", "red","yellow", "green")) +
  xlab("Time") + ylab("Parameter")


plot3



#Question(c)

#Regression splines wtih autoregressive process

sp.autoreg <- function(Y, X, knots, degree){
  #Y: response
  #X: covariate
  #knots: number of knots
  #degree: degree of spline
  
  order = degree + 1 #order
  
  knots = seq(min(X), max(X), length.out = knots+2)
  knots = c(min(X) - (order-1):1 , knots, max(X) + 1:(order-1)) 
  # Basis matrix of B-splines
  N = spline.des(knots, X, ord = order)$design
  
  # autoregressive process
  a = 0.55
  n = length(Y)
  R = toeplitz(a**(0:(n-1)))
  R.inverse = solve(R)
  NRN = t(N) %*% R.inverse %*% N
  NRY = t(N) %*% R.inverse %*% Y
  M = solve(NRN) %*% NRY
  
  #Regression spline
  f.hat = function(x){
    n = spline.des(knots, x, ord = order,  outer.ok = TRUE)$design
    as.numeric(n %*% M)
  }
  return(f.hat)
}

#GCV with autoregressive process

gcvauto <- function(knots, degree){
  fitauto = sp.autoreg(Y, X, knots, deg = degree)
  fitauto = fitauto(X)
  
  a = 0.55
  n = length(Y)
  R = toeplitz(a**(0:(n-1)))
  R.inverse <- solve(R)
  
  aux = t(Y - fitauto) %*% R.inverse %*% (Y - fitauto)
  n = length(Y)
  
  residual = aux/(1 - knots/n)**2
  
  return(residual)
}
GCV <- Vectorize(gcvauto, vectorize.args = c("knots"))
#updated model
gcvauto1 <- which(GCV(1:max_knots, degree = 1) == min(GCV(1:max_knots, degree = 1)))
gcvauto2 <- which(GCV(1:max_knots, degree = 2) == min(GCV(1:max_knots, degree = 2)))
gcvauto3 <- which(GCV(1:max_knots, degree = 3) == min(GCV(1:max_knots, degree = 3)))
gcvauto4 <- which(GCV(1:max_knots, degree = 4) == min(GCV(1:max_knots, degree = 4)))

gcvauto <- data.frame(degree1 = gcvauto1, degree2 = gcvauto2,degree3 = gcvauto3, degree4 = gcvauto4)

gcvauto

# Estimators with updated GCV
fitauto1 <- sp.autoreg(Y, X, knots = gcvauto1, degree = 1)
fitauto2 <- sp.autoreg(Y, X, knots = gcvauto2, degree = 2)
fitauto3 <- sp.autoreg(Y, X, knots = gcvauto3, degree = 3)
fitauto4 <- sp.autoreg(Y, X, knots = gcvauto4, degree = 4)

#Data frame for the plot
df13 <- data.frame(x = Time, y =fitauto1(Time))
df14 <- data.frame(x = Time, y = fitauto2(Time))
df15 <- data.frame(x = Time, y = fitauto3(Time))
df16 <- data.frame(x = Time, y = fitauto4(Time))

#Plot
plot4<- ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df13, aes(color = "1")) +
  geom_line(data = df14, aes(color = "2")) +
  geom_line(data = df15, aes(color = "3")) +
  geom_line(data = df16, aes(color = "4")) +
  scale_colour_manual(name = "Poly.Degree", values = c("blue", "red","yellow" ,"green")) +
  ggtitle("Non parametric smooth lines")+
  xlab("Time") +ylab("Parameter") 

plot4


# Fit parametric model of degree 4
par.fit1 <- lm(Y ~ X + I(X**2) + I(X**3) + I(X**4))

poly.fit1 <- function(x){ as.numeric(par.fit1$coefficients %*% x**(0:4)) }
poly.fit1 <- Vectorize(poly.fit1)

plot5 <- ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df13, aes(color = "Degree 1")) +
  geom_line(data = df14, aes(color = "Degree 2")) +
  geom_line(data = df15, aes(color = "Degree 3")) +
  geom_line(data = df16, aes(color = "Degree 4")) +
  stat_function(fun = poly.fit1) +
  scale_colour_manual(name = "Poly.Degree  ",values = c("blue","red", "yellow","green",'purple')) +
  ggtitle("Non parametric smooth lines")+
  xlab("Time") + ylab("Parameter") 

plot5


# For degree 3 and 5
par.fit2 <- lm(Y ~ X + I(X**2) + I(X**3))
par.fit3 <- lm(Y ~ X + I(X**2) + I(X**3) + I(X**4) + I(X**5))

poly.fit2 <- function(x){ as.numeric(par.fit2$coefficients %*% x**(0:3)) }
poly.fit3 <- function(x){ as.numeric(par.fit3$coefficients %*% x**(0:5)) }
poly.fit2 <- Vectorize(poly.fit2)
poly.fit3 <- Vectorize(poly.fit3)

plot6 <- ggplot(data = data.frame(x = X, y = Y), aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  stat_function(fun = poly.fit1, aes(color = "4")) +
  stat_function(fun = poly.fit2, aes(color = "3")) +
  stat_function(fun = poly.fit3, aes(color = "5")) + 
  scale_colour_manual(name = "Poly.Degree ", values = c("green", "orange","blue")) +
  ggtitle("Non parametric smooth lines")+
  xlab("Time") + ylab("Parameter")

plot6



