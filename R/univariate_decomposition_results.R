library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)

library(mved)

# multivariate(nlsy_simplev2_min_pp)

labinc = nlsy_simplev2_min_pp %>% select(starts_with("labinc"))
wt = nlsy_simplev2_min_pp %>% select(starts_with("wt"))

declabinc <- univariate(labinc)
decwt <- univariate(wt)

plot.ts(declabinc)
plot.ts(decwt)

write.csv(decwt,"F:/Users/Möebius/Documents/GitHub/mved/Docs/Result B/VARS/decwt.csv", row.names = FALSE)
