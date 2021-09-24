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

write.csv(dbyear,"F:/Users/Möebius/Documents/GitHub/mved/data/v2/dbyear.csv", row.names = FALSE)
