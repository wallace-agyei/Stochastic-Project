setwd("C:/Users/wallace/Desktop/StochasticLab2")

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyverse) 
set.seed(42)

#Question (a)

#loading the data
data<- read.csv("StudentsPerformance.csv")

#Implement kernel density
k<- function(x){ifelse((abs(x)<=1),1,0)}
#kernels
epanechnikov <-function(x){(0.75*(1-x**2))*k(x)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((-x**2)/2))}
uniform <- function(x){0.5*k(x)}
triangular <- function(x){(1-abs(x))*k(x)}

kernel.names <- list("Epa","Gauss","Uni", "Tri")
kernel.funtions <- list(epanechnikov, gaussian,uniform, triangular)

#kernel density(kd)
kd <- function(sample, bw, kernel){
  K <- kernel.funtions[[which(kernel.names == kernel)]]
  fxh <- function(x){
    t <- (x - sample)/bw
    (1/(bw*length(sample)))*(sum(K(t)))
  }
  return(Vectorize(fxh))
}

students <- data %>%
  dplyr::select(c(test.preparation.course, math.score, reading.score, writing.score))

#Different bandwidth with epanechnikov kernel
epa1 <- kd(students$math.score, 3, "Epa")
epa2 <- kd(students$math.score, 6, "Epa")
epa3<- kd(students$math.score, 9, "Epa")
epa4 <- kd(students$math.score, 12, "Epa")

plot1<- ggplot(students, aes(x = math.score)) + 
  stat_function(fun = epa1, aes(colour = "3")) +
  stat_function(fun = epa2, aes(colour = "6")) +
  stat_function(fun = epa3, aes(colour = "9")) +
  stat_function(fun = epa4, aes(colour = "12")) +
  ggtitle("Density Plot of different Bandwidth")+
  theme(legend.justification=c(0,0), legend.position=c(0,0),legend.title = element_text()) +
  labs(color = "Bandwidth") #
plot1

#Same bandwidth = 10, with different kernels
epa <- kd(students$math.score, 10, "Epa")
uni <- kd(students$math.score, 10, "Uni")
tri <- kd(students$math.score, 10, "Tri")
gauss<- kd(students$math.score, 10, "Gauss")

plot2<- ggplot(students, aes(x = math.score)) + 
  stat_function(fun = epa, aes(colour = "epanechnikov")) +
  stat_function(fun = uni, aes(colour = "uniform")) +
  stat_function(fun = tri, aes(colour = "triangular")) +
  stat_function(fun = gauss, aes(colour = "gaussian")) +
  ggtitle("Density Plots of Same Bandwidth")+
  theme(legend.justification=c(0,0), legend.position=c(0,0), legend.title = element_text()) +
  labs(color = "Kernel")
plot2

#Question(b)

#Implement the cross-validation
math = students$math.score
reading = students$reading.score
writing = students$writing.score


#Using epanechnikov density as default

CV= function(sample, bw){
  n = length(sample)
  
  func = kd(sample, bw, "Epa")
  func.sq = function(x){func(x)**2} 
  
  cv1 = integrate(func.sq, lower = Inf, upper = Inf)# first term of the expression
  cv2= (sum(epanechnikov(outer(sample, sample, FUN = "-")/bw)) - n*epanechnikov(0))*2/(n*(n-1)*bw)#second term
  
  cv3= cv1$value - cv2
  return(cv3)
}


math.cv = Vectorize(function(bw){CV(math, bw)})
reading.cv = Vectorize(function(bw){CV(reading, bw)})
writing.cv = Vectorize(function(bw){CV(writing, bw)})

# Bandwidth optimization
math.opt = optimize(math.cv, interval = c(1, 20))$minimum
reading.opt = optimize(reading.cv, interval = c(1, 15))$minimum
writing.opt = optimize(writing.cv, interval = c(1, 20))$minimum

#with bw.ucv and bw.bcv
#bw.ucv
bw.ucv.math.score = bw.ucv(math, lower = 1, upper = 20)
bw.ucv.reading.score = bw.ucv(reading, lower = 1, upper = 15)
bw.ucv.writing.score = bw.ucv(writing, lower = 1, upper = 20)

#bw.bcv
bw.bcv.math.score = bw.bcv(math, lower = 1, upper = 20)
bw.bcv.reading.score = bw.bcv(reading, lower = 1, upper = 15)
bw.bcv.writing.score = bw.bcv(writing, lower = 1, upper = 20)

#Question(c)
cpl.studts<- filter(students, test.preparation.course == "completed")
non.studts <- filter(students, test.preparation.course == "none")


#Math Score
#Math Score completed

cpl.math = cpl.studts$math.score
cpl.math.cv = Vectorize(function(bw){CV(cpl.math, bw)})
cpl.math.opt.cv = optimize(cpl.math.cv, interval = c(1, 20))$minimum #optimal bandwidth
msc <- kd(cpl.studts$math.score, cpl.math.opt.cv,"Gauss")#density

#Math Score none complted

non.math = non.studts$math.score
non.math.cv = Vectorize(function(bw){CV(non.math, bw)})
non.math.opt.cv = optimize(non.math.cv, interval = c(1, 20))$minimum #optimal bandwidth
msnc <- kd(non.studts$math.score, non.math.opt.cv, "Gauss")
#plot
plot3 <- ggplot(students, aes(x = math.score)) + 
  stat_function(fun = msc, aes(color = "completed")) +
  stat_function(fun = msnc, aes(color = "none")) +
  scale_colour_manual(name="Test preparation", values = c("red", "blue")) +
  ggtitle("Math score density")+
  theme(legend.justification=c(0,0), legend.position=c(0,0), legend.title = element_text()) 

plot3


#Reading Score

#Reading Score completed
cpl.reading = cpl.studts$reading.score
cpl.reading.cv = Vectorize(function(bw){CV(cpl.reading, bw)})
cpl.reading.opt.cv = optimize(cpl.reading.cv, interval = c(1, 15))$minimum
rsc<- kd(cpl.studts$reading.score, cpl.reading.opt.cv,"Gauss")
#Reading Scone None
non.reading = non.studts$reading.score
non.reading.cv = Vectorize(function(bw){CV(non.reading, bw)})
non.reading.opt.cv = optimize(non.reading.cv, interval = c(1, 15))$minimum
rsnc<- kd(non.studts$reading.score, non.reading.opt.cv, "Gauss")
#plot
plot4 <- ggplot(students, aes(x = reading.score)) + 
  stat_function(fun = rsc, aes(color = "completed")) +
  stat_function(fun = rsnc, aes(color = "none")) +
  scale_colour_manual(name="Test preparation", values = c("red", "blue")) +
  ggtitle("Reading score density")+
  theme(legend.justification=c(0,0), legend.position=c(0,0), legend.title = element_text())

plot4


#Writing Score

#Writing Score completed
cpl.writing = cpl.studts$writing.score
cpl.writing.cv = Vectorize(function(bw){CV(cpl.writing, bw)})
cpl.writing.opt.cv = optimize(cpl.writing.cv, interval = c(1, 20))$minimum
wsc<- kd(cpl.studts$writing.score, cpl.writing.opt.cv,"Gauss")

#Writing Scone None
non.writing = non.studts$writing.score
non.writing.cv = Vectorize(function(bw){CV(non.writing, bw)})
non.writing.opt.cv = optimize(non.writing.cv, interval = c(1, 20))$minimum
wsnc <- kd(non.studts$writing.score, non.writing.opt.cv, "Gauss")

#plot
plot5<- ggplot(students, aes(x = writing.score)) + 
  stat_function(fun = wsc, aes(color = "completed")) +
  stat_function(fun = wsnc, aes(color = "none")) +
  scale_colour_manual(name="Test preparation", values = c("red", "blue")) +
  ggtitle("Writing score density")+
  theme(legend.justification=c(0,0), legend.position=c(0,0), legend.title = element_text())
plot5