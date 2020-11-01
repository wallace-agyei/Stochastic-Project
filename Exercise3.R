setwd("C:/Users/wallace/Desktop/StochasticLab2")

#install.packages(c("JoSAE", "mapdata", "maps", "lme4"))
library(tidyverse)
library(rgdal)
library(maptools)
library(Matrix)
library(mapdata)
library(maps)
library(nlme)
library(ggrepel)
library(JoSAE)
library(conflicted)

set.seed(42)

#Question(a)

#Load the Data
data(landsat)
satdata <-  dplyr::select(landsat, -outlier)

#Quick look at what the data looks
head(landsat)
tail(landsat)
summary(landsat)

#Creat groupedData
corn_group <- groupedData(HACorn ~ PixelsCorn | CountyName, data = landsat)
soy_group <-  groupedData(HASoybeans ~ PixelsSoybeans | CountyName, data = landsat)

# Linear Model for Corn group
lm.corn <- lmList(corn_group)
lm.corn
plot1 <- plot(lm.corn)
plot1



# Linear Model for Soyabeans group
lm.soy <- lmList(soy_group)
lm.soy
plot2 <- plot(lm.soy)
plot2



#Question(b)

#Fitting linear mixed model for both crops so that segments share the same countywide random effect

# Linear Mixed Model for Corn group
lmm.corn= lme(HACorn ~ PixelsCorn, data = corn_group, random = ~ 1)
lmm.corn
summary(lmm.corn)
corn.beta <- lmm.corn$coefficients$fixed
corn.beta
plot3 <- plot(lmm.corn,HACorn~fitted(.)|CountyName,abline=c(0,1))
plot3



# Linear Mixed Model for Soyabeans group
lmm.soy <- lme(HASoybeans ~ PixelsSoybeans, data = soy_group, random = ~ 1)
lmm.soy
summary(lmm.soy)
soy.beta <- lmm.soy$coefficients$fixed
soy.beta
plot4 <- plot(lmm.soy,HASoybeans~fitted(.)|CountyName,abline=c(0,1))
plot4


#Question(c)

#Population for explanatory variables
corn.mean <- unique(satdata$MeanPixelsCorn)
soy.mean <- unique(satdata$MeanPixelsSoybeans)

# Mean of over observed segments only
segments.mean = aggregate(satdata[3:6], by = list(satdata$CountyName), mean)


#Number of observations by county
num_observed_county <- plyr::count(satdata, "CountyName")
county_names <- num_observed_county[, 1]
num_observed <-num_observed_county[, 2]

#var and sigma parameters of both crops
corn_var_estim <- VarCorr(lmm.corn)
soy_var_estim <- VarCorr(lmm.soy)
corn_sigma_estim <- as.numeric(corn_var_estim[2])
soy_sigma_estim <- as.numeric(soy_var_estim[2])
sigma_rand_corn <- as.numeric(corn_var_estim[1])
sigma_rand_soy <- as.numeric(soy_var_estim[1])
#The covariance matrix V_hat of beta_hat
cov_matrix_corn<- list()
cov_matrix_soy <- list()
for (i in 1:12){
  cov_matrix_corn[[i]] = matrix(sigma_rand_corn, num_observed[i], num_observed[i])
  cov_matrix_soy[[i]] = matrix(sigma_rand_soy, num_observed[i], num_observed[i])
}

#Parameters for the estimates
corn_iden_matrix <-  diag(x = corn_sigma_estim, nrow = sum(num_observed), ncol = sum(num_observed))
soy_iden_matrix <- diag(x = soy_sigma_estim, nrow = sum(num_observed), ncol = sum(num_observed))
block_cov_matrix_corn <- bdiag(cov_matrix_corn)
block_cov_matrix_soy <- bdiag(cov_matrix_soy)
V_corn <- corn_iden_matrix + block_cov_matrix_corn
V_soy <- soy_iden_matrix + block_cov_matrix_soy

corn_aux<- cbind(1, satdata$PixelsCorn)
soy_aux<- cbind(1, satdata$PixelsSoybeans)
corn_V_hat <- solve(t(corn_aux) %*% solve(V_corn) %*% corn_aux)
soy_V_hat <- solve(t(soy_aux) %*% solve(V_corn) %*% soy_aux)

corn_gamma <- sigma_rand_corn/(sigma_rand_corn + corn_sigma_estim/num_observed)
soy_gamma <- sigma_rand_soy/(sigma_rand_corn + corn_sigma_estim/num_observed)

#Predictors

#regression predictor:
corn_reg_pred <- cbind(1, corn.mean) %*% corn.beta
corn_reg_pred
soy_reg_pred <- cbind(1, soy.mean) %*% soy.beta

#adjusted survey predictor:
corn_adj_surv_pred <- cbind(1, corn.mean) %*% corn.beta +
  (segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% corn.beta))
soy_adj_surv_pred <- cbind(1, soy.mean) %*% soy.beta +
  (segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% soy.beta))

#Empirical BLUP (EBLUP)
corn_BLUP_pred <- cbind(1, corn.mean) %*% corn.beta +
  corn_gamma*(segments.mean$HACorn - (cbind(1, segments.mean$PixelsCorn) %*% corn.beta))
soy_BLUP_pred <- cbind(1, soy.mean) %*% soy.beta +
  soy_gamma*(segments.mean$HASoybeans - (cbind(1, segments.mean$PixelsSoybeans) %*% soy.beta))

#Survey predictor:
corn_surv_pred <- segments.mean$HACorn
soy_surv_pred <- segments.mean$HASoybeans


#Mean Squared Error(mse) for predictors

#Parameters:
#Let denote predictors by k i.e.: 
#k=0(regression predictor); 
#k=1(adjusted survey predictor);
#k=gamma(Empirical BLUP) 
#k=3(survey predictor)
#crop= corn or soy

MSE_pred = function(k, crop){ 
  if (length(k) == 1){
    k = rep(k, 12)
  }
  if (crop == "corn"){
    sigma.estimate = corn_sigma_estim 
    sigma.rand = sigma_rand_corn 
    gamma = corn_gamma
    crop.mean = corn.mean
    segments.mean = segments.mean$PixelsCorn
    V.hat = corn_V_hat} 
  else {
    sigma.estimate = corn_sigma_estim
    sigma.rand = sigma_rand_soy
    gamma = soy_gamma
    crop.mean = soy.mean
    segments.mean = segments.mean$PixelsSoybeans
    V.hat = soy_V_hat
  }
  
  res = rep(0, 12)
  for (i in 1:12){
    if (k[1] == 2){
      aux = (cbind(1, crop.mean[i]) - cbind(1, segments.mean[i]))
      
      res[i] = sigma.estimate/num_observed[i] + aux %*% V.hat %*% t(aux)
    } else {
      aux1 = (cbind(1, crop.mean[i]) - k[i]*cbind(1, segments.mean[i]))
      aux2 = cbind(1, segments.mean[i])
      
      eqn.1 = (1 - k[i])^2*sigma.rand + k[i]^2*sigma.estimate/num_observed[i]
      eqn.2 = 2*(k[i] - gamma[i])*aux1 %*% V.hat %*% t(aux2)
      eqn.3 = aux1 %*% V.hat %*% t(aux1)
      
      res[i] = eqn.1 + eqn.2 + eqn.3
    }
  }
  return(res)
}

corn_reg_pred_MSE<- MSE_pred(0, crop = "corn")
soy_reg_pred_MSE <- MSE_pred(0, crop = "soy")
corn_adj_surv_pred_MSE <- MSE_pred(1, crop = "corn")
soy_adj_surv_pred_MSE <- MSE_pred(1, crop = "soy")
corn_BLUP_pred_MSE <- MSE_pred(corn_gamma, crop = "corn")
soy_BLUP_pred_MSE <- MSE_pred(soy_gamma, crop = "soy")
corn_surv_pred_MSE <- MSE_pred(2, crop = "corn")
soy_surv_pred_MSE <- MSE_pred(2, crop = "soy")




# Creating a dataframe for Regression predictor

regression_predictor<- data.frame(County = county_names,
                     corn_predictor= corn_reg_pred,
                     corn_MSE = corn_reg_pred_MSE,
                     soy_predictor = soy_reg_pred,
                     soy_MSE= soy_reg_pred_MSE)
view(regression_predictor)

# Creating a dataframe for Adjusted survey predictor

adjusted_survey_predictor<- data.frame(County = county_names,
                                  corn_predictor= corn_adj_surv_pred,
                                  corn_MSE = corn_adj_surv_pred_MSE,
                                  soy_predictor = soy_adj_surv_pred,
                                  soy_MSE= soy_adj_surv_pred_MSE)

view(adjusted_survey_predictor)


# Creating a dataframe for BLUP

blup<- data.frame(County = county_names,
                corn_predictor= corn_BLUP_pred,
                corn_MSE = corn_BLUP_pred_MSE,
                soy_predictor = soy_BLUP_pred,
                soy_MSE= soy_BLUP_pred_MSE)

view(blup)

survey_predictor<- data.frame(County = county_names,
                           corn_predictor= corn_surv_pred,
                           corn_MSE =  corn_surv_pred_MSE,
                           soy_predictor = soy_surv_pred,
                           soy_MSE= soy_surv_pred_MSE)
view(survey_predictor)




#Question(d)

#Estimate the total county field size for both crops
corn_segments_in_county <- unique(satdata$MeanPixelsCorn)
soy_segments_in_county<- unique(satdata$MeanPixelsSoybeans)

#Estimate the total county field size
total_corn_BLUP <- corn_BLUP_pred * corn_segments_in_county
total_soy_BLUP <- soy_BLUP_pred * soy_segments_in_county
total_corn_surv <- corn_surv_pred * corn_segments_in_county
total_soy_surv <- soy_surv_pred * soy_segments_in_county

#Creating a dataframe for total BLUP and total Survey Predictor
df_total <- data.frame(County = county_names, 
                       corn_BLUP = total_corn_BLUP,
                       soy_BLUP = total_soy_BLUP,
                       corn_surv = total_corn_surv,
                       soy_surv = total_soy_surv)
view(df_total)

#Fix the table on the map of Iowa
states <- map_data("state")
iowa <- subset(states, region == "iowa")
counties <- map_data("county")
iowa_county <- subset(counties, region == "iowa")


# Other libraries have select and filter conflicted 
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

iowa_county_polygon <- select(iowa_county, long, lat, subregion)
centroids <- aggregate(iowa_county_polygon[,1:2], by=list(iowa_county_polygon$subregion), FUN = mean)
centroids$County <- unique(iowa_county_polygon[,3])
centroids <- filter(centroids, County %in% tolower(as.character(county_names)))


#Create a dataframe to categorize the crops

#Corn Category
corn_cat <- data.frame(total_blup = "blup:", blup = round(df_total$corn_BLUP, 0),
                       total_survey = "survey:", survey = round(df_total$soy_surv, 0))
corn_cat <- data.frame(blup = paste("", corn_cat$total_blup, "", corn_cat$blup),
                       survey = paste(corn_cat$total_survey, corn_cat$survey))
corn_cat <- data.frame(Total_crop = paste("", corn_cat$blup, "\n", corn_cat$survey))


#Soyabeans Category
soy_cat <- data.frame(total_blup = "blup:", blup = round(df_total$soy_BLUP, 0),
                      total_survey = "survey:", survey = round(df_total$soy_surv, 0))
soy_cat <-  data.frame(blup = paste("", soy_cat$total_blup, "", soy_cat$blup),
                       survey = paste(soy_cat$total_survey, soy_cat$survey))
soy_cat <- data.frame(Total_crop = paste("", soy_cat$blup, "\n", soy_cat$survey))

iowa_county$fill_value = 0
iowa_county$fill_value[iowa_county$subregion %in% tolower(as.character(county_names))] = 1


#Map for corn category
##Corn Map

plot5 <- ggplot(data = iowa_county, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "lightblue", color = "purple") +
  geom_polygon(data = iowa_county, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_label_repel(data = centroids, aes(long, lat, label = corn_cat$Total_crop),
                   size = 3.5, alpha = 0.7, point.padding = 1.5,
                   min.segment.length = 0, segment.size = 0.6) +
  ggtitle("Iowa Map of Predicted Total County Field Size for Corn") +
  scale_fill_manual(values = c("grey","lightblue")) 
plot5



##Soy Map

plot6 <- ggplot(data = iowa_county, aes(x = long, y = lat), fill = ) + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               fill = "lightblue", color = "purple") +
  geom_polygon(data = iowa_county, aes(x = long, y = lat, group = group, fill = factor(fill_value)), 
               color = "white", show.legend = FALSE) +
  geom_polygon(data = iowa, aes(x = long, y = lat, group = group), 
               color = "black", fill = NA) +
  geom_label_repel(data = centroids, aes(long, lat, label = soy_cat$Total_crop),
                   size = 3.5, alpha = 0.7, point.padding = 1.5,
                   min.segment.length = 0, segment.size = 0.6) +
  ggtitle("Iowa Map of Predicted Total County Field Size for Soybeans") +
  scale_fill_manual(values = c("grey","lightblue")) 
plot6


