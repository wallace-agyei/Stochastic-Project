setwd("C:/Users/wallace/Desktop/StochasticLab2")

library(tidyverse)
library(ggplot2)
library(Matrix)
library(pls)
library(tidyverse)
library(dplyr)

#Question(a)
users <- read.csv("users.csv")
head(users)
likes <- read.csv("likes.csv")
head(likes)
users.likes <- read.csv("users-likes.csv")
head(users.likes)



users_row <- match(users.likes$userid, users$userid)

likes_row <- match(users.likes$likeid, likes$likeid)

users.likes <- cbind(users.likes, users_row, likes_row)
head(users.likes)
#Matrix
M <- sparseMatrix(i = users.likes$users_row, j = users.likes$likes_row, x = 1)
N <- M 

rownames(M) <- users$userid
colnames(M) <- likes$name

repeat {
  i <- sum(dim(M))
  M <- M[rowSums(M) >= 80, colSums(M) >= 150]
  if (sum(dim(M)) == i) break
}
users <- users[match(rownames(M),users$userid), ]

likes <- likes[match(colnames(M),likes$likeid), ]

users.likes.M <- as.matrix(M)

head(users.likes.M)

#Question(b)
#Creating train and test sets from the data
set.seed(1122)
n <- nrow(users.likes.M)
train_index <- sample(1:n, size = round((2/3)*n ))
train <- users.likes.M[train_index, ]
test <- users.likes.M[-train_index, ]

train.age <- users$age[train_index]
test.age <- users$age[-train_index]


#fitting data in the regression
model1 <- plsr(train.age ~ train, ncomp = 50)

#prediction on the test set
predict_age1 <- predict(model1, newdata = test)


corr <- 0
for (i in 1:50) {
  corr[i] <- cor(predict_age1[,1,i], test.age, method = "pearson")
}



#d_opt
d_opt <- which(corr == max(corr))
d_opt
#plot
predicted_age <-predict_age1[ , ,d_opt]
df <- data.frame(predicted_age, test.age)


plot1 <- ggplot(df, aes(x = predicted_age, y =  test.age)) +
  geom_point() +
  geom_abline( color = "black") +
  expand_limits(x = 0, y = 0) #To force the origin at 0
plot1

#Question (c)
x <- tail(sort(model1$coefficients[ ,1 , d_opt]), 6)
y <- head(sort(model1$coefficients[ ,1 , d_opt]), 6)

#Question (d) 

users <- read.csv("users.csv")
likes <- read.csv("likes.csv")
users.likes <- read.csv("users-likes.csv")

users_row <- match(users.likes$userid, users$userid)

likes_row <- match(users.likes$likeid, likes$likeid)

users.likes <- cbind(users.likes,users_row, likes_row)

#Matrix
N<- sparseMatrix(i = users.likes$users_row, j = users.likes$likes_row, x = 1)

rownames(N) <- users$userid
colnames(N) <- likes$name

repeat {
  i <- sum(dim(N))
  N <- N[rowSums(N) >= 60, colSums(N) >= 120]
  if (sum(dim(N)) == i) break
}

users <- users[match(rownames(N),users$userid), ]

likes <- likes[match(colnames(N),likes$likeid), ]

users.likes.N <- as.matrix(N)
# this code is exactly like the one discussed above


n <- nrow(users.likes.M)
train_index <- sample(1:n, size = round((2/3)*n ))
train <- users.likes.M[train_index, ]
test <- users.likes.M[-train_index, ]

train.age <- users$age[train_index]
test.age <- users$age[-train_index]


#fitting data in the regression
model2 <- plsr(train.age ~ train, ncomp = 50)

#prediction on the test set
predict_age2 <- predict(model2, newdata = test)


corr <- 0
for (i in 1:50) {
  corr[i] <- cor(predict_age2[,1,i], test.age, method = "pearson")
}

#opt
d_opt <- which(corr == max(corr))
d_opt
predicted_age <-predict_age2[ , ,d_opt]
df <- data.frame(predicted_age, test.age)

plot2 <- ggplot(df, aes(x = predicted_age, y =  test.age)) +
  geom_point() +
  geom_abline( aes(intercept=0, slope=1), size = 1) +
  scale_colour_manual(name = " ", values = c("red")) +
  expand_limits(x = 0, y = 0)


plot2
#Question (c)
m <- tail(sort(model2$coefficients[ ,1 , d_opt]), 6)
n <- head(sort(model2$coefficients[ ,1 , d_opt]), 6)


