# Multi-variate Earning Dynamics
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


uved <- function(x) {

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

  fitMARIMA <- marima(x)

  resMARIMA = resid(fitMARIMA)

  preTit = resMARIMA

  # Transitory Component

  Tit <- as.data.frame(t(preTit))

  # Getting an ARIMA model from the MARIMA.

  tam = nrow(x)

  fit1 <- arima.sim(list(ar=fitMARIMA$coef["ar1"],ma=fitMARIMA$coef["ma1"]),tam)

  #Random Walk

  RW <- fit1

  RW_diff <- diff(RW)

  mean(RW_diff)
  sd(RW_diff)

  RW_drift <- arima.sim(model= list(ar=fitMARIMA$coef["ar1"],ma=fitMARIMA$coef["ma1"]), n=tam, mean=1,sd=5)

  RW_drift_diff <- diff(RW_drift)

  #Permanent Component

  Pit <- RW_drift_diff

  # Earning Dynamics Equation

  Pi <- head(Pit,40)
  Ti <- head(Tit,40)

  modeldata <- data.frame(Pi,Ti)

  F_temp <- modeldata
  return(F_temp);

}

mved <- function(x) {

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

  fitMARIMA <- marima(x)

  resMARIMA = resid(fitMARIMA)

  preTit = resMARIMA

  # Transitory Component

  Tit <- as.data.frame(t(preTit))

  # Getting an ARIMA model from the MARIMA.

  tam = nrow(Tit)

  fit1 <- arima.sim(list(ar=fitMARIMA$coef["ar1"],ma=fitMARIMA$coef["ma1"]),tam)

  #Random Walk

  RW <- fit1

  RW_diff <- diff(RW)

  mean(RW_diff)
  sd(RW_diff)

  RW_drift <- arima.sim(model= list(ar=fitMARIMA$coef["ar1"],ma=fitMARIMA$coef["ma1"]), n=tam, mean=1,sd=5)

  RW_drift_diff <- diff(RW_drift)

  #Permanent Component

  Pit <- RW_drift_diff

  # Earning Dynamics Equation

  modeldata <- as.data.frame(Pit,Tit)

  F_temp <- modeldata
  return(F_temp);

}

