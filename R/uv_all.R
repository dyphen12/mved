library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)

library(mved)

# multivariate(nlsy_simplev2_min_pp)

labinc = nlsy_simplev2_min %>% select(starts_with("labinc"))

uvmodeldata <- univariate(labinc)

plot.ts(uvmodeldata)

# write.csv(declabinc,"F:/Users/Möebius/Documents/GitHub/mved/Docs/Result B/VARS/declabinc.csv", row.names = FALSE)
