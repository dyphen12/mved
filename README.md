# MVED: Multi-variate Earning Dynamics Package for R

 A multivariate approach on Income Decomposition

This is a first implementation of the research on Earning Dynamics modeling improvement, which models the dynamics of an earning process. Here we implement an multi-variate approach.

# Equations

 **yit = αi + pit + τit**

**pit = φppit−1 + ξit,**

**τit = θ(L)it**

- **yit:** is individual i’s log-earnings (residuals) at time t;
- **pit:** is the permanent component (random walk if φp = 1);
- **τit:** is the transitory component: MA(1), ARMA(1,1), AR(1), or iid;
- **αi:** is an individual fixed effect

  # MARIMA Model
 1) We pre-train an MARIMA model to obtain a full AR and MA coefficients of the multi-variate process.
 
2) Then, we train the model with p,q,d (1,0,1) as shown in the literacy.

4) We have that a model of the permanent effect of the shock is equivalent to a Random Walk model first order of differentiation, we simulate this with the previously obtained model and thus we obtain this resource.
