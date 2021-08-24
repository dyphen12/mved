# Permanent Component

library(plotly)
library(tseries)
library(ggplot2)
library(MTS)
library(marima)

dataPR <- nlsy_simplev2_filled

#Multi-variate Implementation MARIMA

library(marima)

Model1 <- define.model(kvar=13, ar=c(1, 0), ma=c(1), reg.var=13)

fitMARIMA <- marima(dataPR, ar.pattern = Model1$coef["ar1"], ma.pattern = Model1$coef["ma1"])

resMARIMA = resid(fitMARIMA)

preTit = resMARIMA

# Transitory Component

Tit <- as.data.frame(t(preTit))

tam = nrow(Tit)

tam = tam+1

#Random Walk

#fit1 <- marima.sim(list(ar=fitMARIMA$coef["ar0"],ma=fitMARIMA$coef["ma0"]),tam)

RWmodel <- marima.sim(kvar = 13, ar.model = fitMARIMA$coef["ar0"], ma.model = fitMARIMA$coef["ma0"], nsim = tam,  averages = fitMARIMA$averages)

RW <- diff(RWmodel)

#Permanent Component

Pit <- RW

# write.csv(Pit,"F:/Users/Möebius/Documents/Prisma/Christos Makridis/Earning Dynamics Package/mved/data/Pit_simple.csv", row.names = FALSE)
# write.csv(tran,"F:/Users/Möebius/Documents/GitHub/mved/data/Tit(nlsy_min).csv", row.names = FALSE)

# Earning Dynamics Equation

modeldata <- merge.data.frame(Pit,Tit)

F_temp <- modeldata
