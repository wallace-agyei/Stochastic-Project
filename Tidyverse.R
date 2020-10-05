
library(tidyverse)
library(foreign)    # Library for reading STATA files into R
library(maptools)
library(raster)
library(plyr)
library(dplyr)
library(ggplot2)
library(rgdal)


setwd("C:/Users/wallace/Desktop/StochasticLab2")
set.seed(42)

#Question(a)
childrenData = read.dta("childrenfinal.dta")
childrenData

#Remove variables starting with s,v,m with numbers
childrenData1<- childrenData %>% 
  dplyr::select(-matches("^[svm][0-9]"))
view(childrenData1)

#Change data type as a factor
childrenData1$female <- as.factor(childrenData1$female)
childrenData1$ruralfacto <-  as.factor(childrenData1$ruralfacto)

#Question(b)
#Creating tibble of some selected variables
childrenData2<- childrenData1 %>% 
  dplyr::select(c(hypage, ruralfacto, female, zstunt, zweight, zwast, adm2))
childrenData2

#Scatter plot of zstunt against hypage

plot1 <- ggplot(data = childrenData2) +
  geom_point(mapping = aes(x = hypage, y = zstunt), color = "black")+
  ggtitle("Scatterplot of Zstunt against Hypage")+
  xlab("Hypage") + ylab("Zstunt")
plot1
ggsave("C:/Users/wallace/Desktop/StochasticLab2/Exercise1plot1.png")




#Smooth plot of zstunt against age for females and males
plot2 <- ggplot(childrenData2, mapping =  aes(x = hypage, y = zstunt, color = female)) +
  geom_point(aes(color = female))+geom_smooth(aes(color = female))+
  scale_color_discrete(name="Sex",labels=c("Male","Female"))+
  labs(color = "Gender") +
  ggtitle("Plot of Zstunt against Hypage by Sex") +
  xlab("Hypage") + ylab("Zstunt")
plot2
ggsave("C:/Users/wallace/Desktop/StochasticLab2/Exercise1plot2.png")



plot3 <- ggplot(childrenData2, mapping =  aes(x = hypage, y = zstunt, color = factor(female, labels = c("Urban", "Rural")))) +
  geom_point(aes(color =ruralfacto))+geom_smooth(aes(color = ruralfacto))+
  scale_color_manual(name="Area",values=c("yellow","purple"),labels=c("Urban","Rural"))+
  labs(color = "Area")+
  ggtitle("Plot of Zstunt against Hypage by Area ") +
  xlab("Hypage") + ylab("Zstunt")
plot3
ggsave("C:/Users/wallace/Desktop/StochasticLab2/Exercise1plot3.png")



#Creating the Kenya Map
Kenya1 <- getData("GADM", country="KE", level=1) 
Kenya1_UTM <- spTransform(Kenya1, CRS("+init=epsg:32737")) # setting an appropriate projection 
colnames(childrenData2)[7] <- "NAME_1"             # To rename the adm column to suit what the kenyan map looks 


#Arranging the County alphabetically
childrenData2 <- childrenData2[order(childrenData2$NAME_1),]
Kenya1_UTM@data <- Kenya1_UTM@data[order(Kenya1_UTM@data$NAME_1),]

#Summarizing the dataset by mean of each county
detach(package:plyr)         ##To deactivate 'plyr' so as to enable group_by
childrenData3 <- childrenData2%>%
  group_by(NAME_1) %>%
  summarize(zstunt.mean = mean(zstunt), n = n())


##Adding the missing county, Isiolo
childrenData3 [nrow(childrenData3) + 1, ] = NA
childrenData3 [47, 1] = "Isiolo" ##Change the created cell of adm2 to Isiolo
view(childrenData3 )


#Creating a dataframe for ggplot
Kenya1_UTM@data$id = rownames(Kenya1_UTM@data)
Kenya1_UTM@data = mutate(Kenya1_UTM@data, zstunt.mean = childrenData3$zstunt.mean)
Kenya1_df = fortify(Kenya1_UTM)
Kenya1_df = full_join(Kenya1_df,Kenya1_UTM@data, by="id")

##In the order listed at Kenya1_UTM@data, we need the centroids of each county
childrenData4 <- as.data.frame(coordinates(Kenya1_UTM))
names(childrenData4) <- c("long", "lat")
childrenData3 <- childrenData3[order(childrenData3$NAME_1),]
childrenData4$NAME_1 <- Kenya1_UTM@data$NAME_1
childrenData4$zstunt.mean <- childrenData3$zstunt.mean

##Generating the map with all the counties and their Zstunt mean

plot4 <- ggplot(data = Kenya1_df, aes(x = long, y = lat, group = group, fill = zstunt.mean)) + 
  geom_polygon(color = "black", size = 0.25) +
  geom_text(data =childrenData4, aes(x = long, y = lat, label = NAME_1, group = NULL), size = 3) +
  scale_fill_distiller(name="Zstunt mean of\nKenyan Counties", palette = "Spectral") +
  ggtitle("Map of Kenya with the Stunt Average of Children age 0 and 5 years")
theme(aspect.ratio=1)
plot4
ggsave("C:/Users/wallace/Desktop/StochasticLab2/Exercise1plot4.png")

##Tibble to text file 
write.table(childrenData2,"C:/Users/wallace/Desktop/StochasticLab2/children2.txt")


