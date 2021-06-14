library(marima)

# Transitory Component:

# Assuming low order MARIMA

fitMARIMA <- marima(nlsy_simplev2_filled)

resMARIMA = resid(fitMARIMA)

preTit = resMARIMA

# Resultant Transitory Component

Tit <- as.data.frame(t(preTit))

plot.ts(Tit)

# write.csv(Tit,"F:/Users/Möebius/Documents/Prisma/Christos Makridis/Earning Dynamics Package/mved/data/Tit.csv", row.names = FALSE)



