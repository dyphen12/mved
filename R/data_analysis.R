library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)
library(marima)
library(dse)

library(zoo)
library(cluster)
library(factoextra)
library(NbClust)
# Data Analysis

str(nlsy_simple)

#plot(nlsy_simple)


dataset <- nlsy_simplev2_filled

#dataset <- scale(dataset)

dec <- decompose(dataset)

autoplot(dec)

sample <- head(dataset,100)

mdistance <- get_dist(sample, method='euclidean')
fviz_dist(mdistance, gradient = list(low ='blue', mid='white',high='red'))

fviz_nbclust(sample,kmeans,method='wss')
fviz_nbclust(sample,kmeans,method='silhouette')
fviz_nbclust(sample,kmeans,method='gap_stat')

resnumclust <- NbClust(sample, distance ='euclidean',min.nc=2,max.nc=10,method='kmeans', index='alllong')

k2 <- kmeans(sample, centers=2,nstart=25)

str(k2)

k2

fviz_cluster(k2, data = sample)

dataset[10,]


# Data Preprocessing

#Order by year

yearnlsy <-nlsy_simple[order(nlsy_simple$year),]

# Load Row from dataset and select Row(variable)

RAWdata = yearnlsy

Row = 'PPamount'

# More Data Preprocessing


