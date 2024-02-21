###########################
### Necessary functions ###
###########################
# Sindy prediction
# Function that predicts the next step of a time series using SINDy
# x: initial condition of time series
# u: initial exogenous parameter
# coeff: coefficients that have been calculated using the training set
sindy_pred = function(x, u, coeff, order) {
  
  xu = data.frame(t(x), u)
  colnames(xu) = c("x", "y", "z", "u")
  features = sindyr::features(xu, polyorder = order)

  pred = c(features %*% coeff[, 1],
           features %*% coeff[, 2],
           features %*% coeff[, 3])

  return(pred)
}


# Integration function
# Function that uses Runge-Kutta to integrate the result of a given function
# for a given amount ot time steps ahead
# v: function the result of which needs to be integrated
# y: initial condition
# u: exogenous parameter of length n
# h: size of time step
# p: coefficients
# o: order of polynomial
rk4u = function(v, y, u, h, p, o) { 
  
  # RK4U   Runge-Kutta scheme of order 4 for control system
  k1 = v(y, u, p, o)
  k2 = v(y + (h/2)*k1, u, p, o)
  k3 = v(y + (h/2)*k2, u, p, o)
  k4 = v(y + h*k3, u, p, o)
  y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6

  return(y)
}


# NRMSE statistic function
# Description: calculates the NRMSE btw real and predictedvalues for a certain amount of observations 
# (used for bootstraping)
# data: dataset with one column for real and one for predicted values
# indices: amount of rows to be used in calculation
# Output: NRMSE value
NRMSE_function <- function(data, indices) {
  
  d <- data[indices, ]
  
  obs_sd = sd(d[, 1])
  pred_errors = (d[, 1] - d[, 2])^2
  
  NRMSE = sqrt(mean(pred_errors))/obs_sd
  
  NRMSE
}
