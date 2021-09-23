library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)

library(mved)

mvedyit <- multivariate(nlsy_simplev2_min)
mvtit = transitorycomp(nlsy_simplev2_min)
mvpit = data.frame(permanentcomp(nlsy_simplev2_min))

mvdeclabinctit = mvtit %>% select(starts_with("u3"))
mvdeclabincpit = mvpit %>% select(starts_with("X3"))
mvdeclabincyit = mvedyit %>% select(starts_with("u3"))

mvedmodeldata = data.frame(mvdeclabinctit,mvdeclabincpit,mvdeclabincyit)

plot.ts(mvedmodeldata)

# write.csv(mdeclabinc,"F:/Users/Möebius/Documents/GitHub/mved/Docs/Result B/VARS/mdeclabinc.csv", row.names = FALSE)

labinc = nlsy_simplev2_min_pp %>% select(starts_with("labinc"))

uvmodeldata <- univariate(labinc)

plot.ts(uvmodeldata)

# write.csv(declabinc,"F:/Users/Möebius/Documents/GitHub/mved/Docs/Result B/VARS/declabinc.csv", row.names = FALSE)
