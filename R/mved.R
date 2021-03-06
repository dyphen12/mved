# Multi-variate Earning Dynamics
#THIS IS THE DEV VERSION!!!
#
# Author: Alexis Wong (@dyphen12).
#
#
# This is a first implementation of the research on Earning Dynamics modeling improvement.
# which models the dynamics of an earning process. Here we implement an multi-variate approach.
#
# Based on the papers:
#
#  1) Daly,Hryshko - Improving measurement of earnings dynamics (Dec 2020).
#
#  2) Hryshko & Manovskii: Greatest Hits and All-Time Favorites (& Some That Will Be) (Oct 2020)
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(forecast)


univariate <- function(x) {

  # Given the equation

  # yit = αi + pit + τit

  # pit = φppit−1 + ξit,

  # τit = θ(L)it

  # yit is individual i’s log-earnings (residuals) at time t;
  # pit is the permanent component (random walk if φp = 1);
  # τit is the transitory component: MA(1), ARMA(1,1),
  # AR(1), or iid;
  # αi is an individual fixed effect


  # Data Load

  process = x

  fit <- Arima(process, order=c(1,0,1))

  res = resid(fit)

  Tit = res

  #RW

  tam = nrow(process)

  RW <- arima.sim(list(ar=fit$coef["ar1"],ma=fit$coef["ma1"]),tam)

  RW_diff <- diff(RW)

  RW_drift <- arima.sim(model= list(ar=fit$coef["ar1"],ma=fit$coef["ma1"]), n=tam, mean=1,sd=5)

  prePit = data.frame(process,RW_drift)
  postPit = prePit %>% replace(is.na(.), 0) %>% mutate(Pit = rowSums(.[1:2]))

  Pit = postPit[3]

  modeldata = data.frame(Pit,Tit)
  ymodeldata = modeldata %>% replace(is.na(.), 0) %>% mutate(Yit = rowSums(.[1:2]))
  Decomposition = ymodeldata[1:3]

  return(ymodeldata);

}

multivariate <- function(x) {

  # Given the equation

  # yit = αi + pit + τit

  # pit = φppit−1 + ξit,

  # τit = θ(L)it

  # yit is individual i’s log-earnings (residuals) at time t;
  # pit is the permanent component (random walk if φp = 1);
  # τit is the transitory component: MA(1), ARMA(1,1),
  # AR(1), or iid;
  # αi is an individual fixed effect


  # Data Load

  # MARIMA Model
  #
  # 1) We train an MARIMA automatic model to obtain a full AR and MA coefficients of the multi-variate process.
  #
  # 2) Then, we train an ARIMA (1,0,1) model to obtain the resources of this model as seen in the paper.
  #
  # 3) We build an autoARIMA + ARIMA (1,0,1) model.
  #
  # 4) We have that a model of the permanent effect of the shock is equivalent to a Random Walk
  #    model first order of differentiation,
  #    we simulate this with the previously obtained model and thus we obtain this resource.

  #Multi-variate Implementation MARIMA

  library(marima)

  dataPR <- x

  tokvar <- ncol(dataPR)

  Model1 <- define.model(kvar=tokvar, ar=c(1, 0), ma=c(1), reg.var=13)

  fitMARIMA <- marima(dataPR, ar.pattern = Model1$coef["ar1"], ma.pattern = Model1$coef["ma1"])

  resMARIMA = resid(fitMARIMA)

  preTit = resMARIMA

  # Transitory Component

  Tit <- as.data.frame(t(preTit))

  tam = nrow(Tit)

  tam = tam+1

  #Random Walk

  RWmodel <- marima.sim(kvar = tokvar, ar.model = fitMARIMA$coef["ar0"], ma.model = fitMARIMA$coef["ma0"], nsim = tam,  averages = fitMARIMA$averages)

  RW <- diff(RWmodel)

  #Permanent Component

  Pit <- RW

  # Earning Dynamics Equation

  Yit <- Pit + Tit

  F_temp <- Yit
  return(F_temp);

}

mvedequation <- function(Pit, Tit){

  Yit <- Pit + Tit

  F_temp <- Yit
  return(F_temp);

}

uvedequation <- function(Pit, Tit){

  modeldata = data.frame(Pit,Tit)
  ymodeldata = modeldata %>% replace(is.na(.), 0) %>% mutate(Yit = rowSums(.[1:2]))
  Decomposition = ymodeldata[1:3]

  return(ymodeldata);

}

transitorycomp <- function(x){

  library(marima)

  dataPR <- x

  tokvar <- ncol(dataPR)

  Model1 <- define.model(kvar=tokvar, ar=c(1, 0), ma=c(1), reg.var=13)

  fitMARIMA <- marima(dataPR, ar.pattern = Model1$coef["ar1"], ma.pattern = Model1$coef["ma1"])

  resMARIMA = resid(fitMARIMA)

  preTit = resMARIMA

  # Transitory Component

  Tit <- as.data.frame(t(preTit))

  return(Tit);

}

permanentcomp <- function(x){

  # Given the equation

  # yit = αi + pit + τit

  # pit = φppit−1 + ξit,

  # τit = θ(L)it

  # yit is individual i’s log-earnings (residuals) at time t;
  # pit is the permanent component (random walk if φp = 1);
  # τit is the transitory component: MA(1), ARMA(1,1),
  # AR(1), or iid;
  # αi is an individual fixed effect


  # Data Load

  # MARIMA Model
  #
  # 1) We train an MARIMA automatic model to obtain a full AR and MA coefficients of the multi-variate process.
  #
  # 2) Then, we train an ARIMA (1,0,1) model to obtain the resources of this model as seen in the paper.
  #
  # 3) We build an autoARIMA + ARIMA (1,0,1) model.
  #
  # 4) We have that a model of the permanent effect of the shock is equivalent to a Random Walk
  #    model first order of differentiation,
  #    we simulate this with the previously obtained model and thus we obtain this resource.

  #Multi-variate Implementation MARIMA

  library(marima)

  dataPR <- x

  tokvar <- ncol(dataPR)

  Model1 <- define.model(kvar=tokvar, ar=c(1, 0), ma=c(1), reg.var=13)

  fitMARIMA <- marima(dataPR, ar.pattern = Model1$coef["ar1"], ma.pattern = Model1$coef["ma1"])

  resMARIMA = resid(fitMARIMA)

  preTit = resMARIMA

  # Transitory Component

  Tit <- as.data.frame(t(preTit))

  tam = nrow(Tit)

  tam = tam+1

  #Random Walk

  RWmodel <- marima.sim(kvar = tokvar, ar.model = fitMARIMA$coef["ar0"], ma.model = fitMARIMA$coef["ma0"], nsim = tam,  averages = fitMARIMA$averages)

  RW <- diff(RWmodel)

  #Permanent Component

  Pit <- RW

  # Earning Dynamics Equation

  return(Pit);

}

