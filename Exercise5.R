setwd("C:/Users/wallace/Desktop/StochasticLab2")


library(readxl)
library(ggplot2)
library(tidyverse) 
library(NonpModelCheck)


set.seed(42)

#loading the data
children <- read.delim("children2.txt", sep = "")
head(children)

#Question(a)

i <- function(x){ifelse((abs(x)<=1),1,0)}
#kernels
nadaraya <- i
uniform <- function(x){0.5*i(x)}
triangular <- function(x){(1-abs(x))*i(x)}
epanechnikov <-function(x){(0.75*(1-x**2))*i(x)}
gaussian <- function(x){((2*pi)^(-1/2))*(exp((-x**2)/2))}

kernel_names <- list("Epa","Gauss","Uni", "Tri","Nada")
kernel_funtions <- list(epanechnikov,gaussian,uniform, triangular,nadaraya)

#local polynomial fit function

local_poly_fit <- function(Y, X, bw, degree, kernel = "Epa"){
  # Y = response variable
  # X = covariate
  # bw = bandwidth
  # order=order of the derivative
  # degree= degree of the polynomial
  # kernel = epanechnikov
  
  
kernel <- kernel_funtions[[which(kernel_names == kernel)]]
Y <- as.matrix(Y)
X <- as.matrix(X)
d <- ncol(X)
n <- nrow(X)
X_mat <- matrix(0, d*n, degree+1)
  
A.hat <- function(x, order){
    # order = order of derivative 
    eqn1 <- (X - x)/bw
    eqn2 <- as.vector(t(X - x))
    
    V <- apply(eqn1, MARGIN = 1, FUN = kernel)
    
    for (i in 0:order){
      X_mat[ ,i+1] <- eqn2^i
    }
    res <- lm(Y ~ X_mat - 1, weights = V) 
    res <- res$coefficients
    res <- factorial(order)*res[order + 1]
    return(res)
  }
  VA.hat <- Vectorize(A.hat, vectorize.args = "x")
  return(VA.hat)
}

#First set the polynomial degree to 1 and estimate with 4 different bandwidths
f1 <-local_poly_fit(children$zwast, children$hypage, bw = 3, degree = 1)
f2 <-local_poly_fit(children$zwast, children$hypage, bw = 6, degree= 1)
f3 <-local_poly_fit(children$zwast, children$hypage, bw = 9, degree= 1)
f4 <-local_poly_fit(children$zwast, children$hypage, bw = 12,degree= 1)

#Fitting on the domain of hypage = 0:59
fit1 <- f1(x = 0:59, order = 0)
fit2 <- f2(x = 0:59, order = 0)
fit3 <- f3(x = 0:59, order= 0)
fit4 <- f4(x = 0:59, order= 0)

#dataframes for ggplot
df1 <- data.frame(a = 0:59, b = fit1)
df2 <- data.frame(a = 0:59, b = fit2)
df3 <- data.frame(a = 0:59, b = fit3)
df4 <- data.frame(a = 0:59, b = fit4)
#Then the plot
plot1<- ggplot(children, aes(x = hypage, y = zwast)) +
  geom_point(color = "lightblue") +
  geom_line(data = df1, aes(x = a, y = b, color = "3"), size = 1) +
  geom_line(data = df2, aes(x = a, y = b, color = "6"), size = 1) +
  geom_line(data = df3, aes(x = a, y = b, color = "9"), size = 1) +
  geom_line(data = df3, aes(x = a, y = b, color = "12"), linetype = "dashed", size = 1) +
  labs(color = "Bandwidth") 

plot1


#now with bw = 10, we fix the polynomial degree to 1, estimate f with 4 kernels from exercise 6
f5 <- local_poly_fit(children$zwast, children$hypage, bw = 10, degree = 1, kernel = "Epa")
f6 <- local_poly_fit(children$zwast, children$hypage, bw = 10, degree = 1, kernel = "Uni")
f7 <- local_poly_fit(children$zwast, children$hypage, bw = 10, degree = 1, kernel = "Tri")
f8 <- local_poly_fit(children$zwast, children$hypage, bw = 10, degree= 1, kernel = "Gauss")
#Fitting on the domain of hypage = 0:59
fit5 <- f5(x = 0:59, order= 0)
fit6 <- f6(x = 0:59, order= 0)
fit7 <- f7(x = 0:59, order= 0)
fit8 <- f8(x = 0:59, order= 0)

#dataframes for ggplot
df5 <- data.frame(a = 0:59, b = fit5)
df6 <- data.frame(a = 0:59, b = fit6)
df7 <- data.frame(a = 0:59, b = fit7)
df8 <- data.frame(a = 0:59, b = fit8)
#Then the plot
plot2<- ggplot(children, aes(x = hypage, y = zwast)) +
  geom_point(color = "lightblue") +
  geom_line(data = df5, aes(x = a, y = b, color = "epanechnikov"), size = 1) +
  geom_line(data = df6, aes(x = a, y = b, color = "uniform"), size = 1) +
  geom_line(data = df7, aes(x = a, y = b, color = "triangular"), size = 1) +
  geom_line(data = df8, aes(x = a, y = b, color = "gaussian"),  size = 0.5) +
  labs(color = "Kernel") 

plot2


#Question (b)
#Write a function that calculates the optimal bandwidth with Generalised Cross Validation (GCV)

gcv <- function(Y, X, bw, degree){
  X_values <- unique(sort(X))
  n <- length(Y)
  X_mat_list <- list()
  for (i in X_values){
    index = which(X_values == i)
    X_mat_list[[index]] = matrix(0, n, 4+1)
    aux = (X - i)
    for (j in 0:4){
      X_mat_list[[index]][, j+1] = aux**j
    }
  }
  
  sum_square = rep(0, length(X_values)) 
  W_trace = matrix(0, length(X_values))
  
  poly_fit = local_poly_reg(Y, X, bw, degree)
  
  
  for (i in X_values){
    index = which(X_values == i)
    aux = (X - i)
    
    X_mat = X_mat_list[[index]][, 1:(degree+1)]
    
    V = diag(epanechnikov(aux/bw))
    
    weight_vector = solve(t(X_mat) %*% V %*% X_mat) %*% t(X_mat) %*% V
    weight_vector = weight_vector[1, ]
    
    W_trace[index] = sum((X == i) * weight_vector)
    
    sum_square[index] = sum((Y[(X == i)] - poly_fit(i, order= 0))**2)
  }
  res = sum(sum_square)/(1 - sum(W_trace)/n)**2
  
  return(res)
  
}

#optimal bandwidth by GCV for polynomial degrees 1 to 4
Y <- children$zwast
X <- children$hypage
gcv1 <- Vectorize(function(bw){gcv(Y, X, bw, degree= 1)})
gcv2 <- Vectorize(function(bw){gcv(Y, X, bw, degree= 2)})
gcv3 <- Vectorize(function(bw){gcv(Y, X, bw, degree= 3)})
gcv4 <- Vectorize(function(bw){gcv(Y, X, bw, degree= 4)})

GCV1 <- optimize(gcv1, interval = c(2, 11))$minimum
GCV2 <- optimize(gcv2, interval = c(2, 11))$minimum
GCV3 <- optimize(gcv3, interval = c(2, 11))$minimum
GCV4 <- optimize(gcv4, interval = c(2, 11))$minimum

#Plot all four fits putting the curves on the same plot
f9 <- local_poly_fit(children$zwast, children$hypage, bw = GCV1, degree= 1, kernel = "Epa")
f10 <- local_poly_fit(children$zwast, children$hypage, bw =GCV2, degree= 2, kernel = "Epa")
f11 <- local_poly_fit(children$zwast, children$hypage, bw =GCV3, degree= 3, kernel = "Epa")
f12 <- local_poly_fit(children$zwast, children$hypage, bw =GCV4, degree= 4, kernel = "Epa")
#Fitting on the domain of hypage = 0:59
fit9 <- f9(x = 0:59,  order= 0)
fit10 <- f10(x = 0:59, order= 0)
fit11 <- f11(x = 0:59, order= 0)
fit12 <- f12(x = 0:59, order= 0)

#dataframes for ggplot
df9 <- data.frame(a = 0:59, b = fit9)
df10 <- data.frame(a = 0:59, b = fit10)
df11 <- data.frame(a = 0:59, b = fit11)
df12 <- data.frame(a = 0:59, b = fit12)


plot3<- ggplot(children, aes(x = hypage, y = zwast)) +
  geom_point(color = "lightblue") +
  geom_line(data = df9, aes(x = a, y = b, color = "1")) +
  geom_line(data = df10, aes(x = a, y = b, color = "2")) +
  geom_line(data = df11, aes(x = a, y = b, color = "3")) +
  geom_line(data = df12, aes(x = a, y = b, color = "4")) +
  labs(color = "poly.degrees")

plot3



#Question(c)
#calculate the first derivative of the function of zwast with the GCV-bandwidth 


derv.1 <- localpoly.reg(X, Y, bandwidth = 4.999956, degree.pol = 1, deriv = 1)
derv.2 <- localpoly.reg(X, Y, bandwidth = 10.99995, degree.pol = 2, deriv = 1)
derv.3 <- localpoly.reg(X, Y, bandwidth = 10.99994, degree.pol = 3, deriv = 1)
derv.4 <- localpoly.reg(X, Y, bandwidth = 8.403991, degree.pol = 4, deriv = 1)
#dataframes for ggplot
df13 <- data.frame(a = unique(derv.1$x), b = unique(derv.1$predict))
df14 <- data.frame(a = unique(derv.2$x), b = unique(derv.2$predict))
df15 <- data.frame(a = unique(derv.3$x), b = unique(derv.3$predict))
df16 <- data.frame(a = unique(derv.4$x), b = unique(derv.4$predict))

#Plot all four derivative fits on one plot
plot4<- ggplot(children, aes(x = hypage, y = zwast)) +
  geom_point(color = "lightblue") +
  geom_line(data = df13, aes(x = a, y = b, color = "1")) +
  geom_line(data = df14, aes(x = a, y = b, color = "2")) +
  geom_line(data = df15, aes(x = a, y = b, color = "3")) +
  geom_line(data = df16, aes(x = a, y = b, color = "4")) +
  labs(color = "poly. degrees")


plot4