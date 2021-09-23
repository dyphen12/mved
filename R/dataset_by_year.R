library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)

library(mved)

# All Dataset ordered by year

data <- nlsy_simplev2_min

dbyear <- data[order(data$year),]

plot.ts(dbyear)
