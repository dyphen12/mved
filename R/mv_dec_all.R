library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)

library(mved)

resultmv <- multivariate(nlsy_simplev2_min)

plot.ts(resultmv)
