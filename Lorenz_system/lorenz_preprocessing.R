library(ggplot2)
library(tidyr)
library("plot3D")
library(boot)
library(PLRModels)
library(sindyr)
library(tseries)
library(lokern)
library(purrr)
library(astsa)


# Function that generates data for Lorenz system
lorenz_system = function(x,u,p) {

  xdot = c(sigma*(-x[1]+x[2])+u,
          rho*x[1]-x[2]-x[1]*x[3],
          -beta*x[3]+x[1]*x[2])

  return(xdot) 
}

# RK4U   Runge-Kutta scheme of order 4 for control system
rk4u = function(v, y, u, h, p) { 
  
  k1 = v(y, u, p)
  k2 = v(y + (h/2)*k1, u, p)
  k3 = v(y + (h/2)*k2, u, p)
  k4 = v(y + h*k3, u, p)
  y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6
  
  return(y)
}

# Paraneters that define the Lorenz system
dt = 0.001
Tfin = 15
tspan = seq(0, Tfin, dt)
x0 = c(-8, 8, 27)
forcing = function(t) sin(4*t)
rho = 28
sigma = 10
beta = 8/3
p = c(rho, sigma, beta)

# Lorenz system data generation
xhat = matrix(0, length(tspan), 4)
xhat[1, ] = c(x0, forcing(tspan[1]))

for (i in 1:(length(tspan)-1)) {
  xhat[i+1, 1:3] = rk4u(lorenz_system, xhat[i, 1:3], forcing(tspan[i+1]), dt, p)
  xhat[i+1, 4] = forcing(tspan[i+1])
}

xyzu = data.frame(x=xhat[, 1], y=xhat[, 2], z=xhat[, 3], u=xhat[, 4])

setwd('C:/Users/evsop/OneDrive/Documents/Master/4th Term/TFM/dynamical_fluid_forecast_TFM/Lorenz')

# 3D plot
scatter3D(as.vector(xyzu$x), as.vector(xyzu$y), as.vector(xyzu$z), surface=FALSE, fill=FALSE)

# Plot xyzu coordinates
ts_xyz = ts(xyzu, start = c(0,0.001),frequency = 1)
par(cex.axis=2,cex.lab=2.2, cex.main=3)
plot.ts(ts_xyz, main="")

# Create training and prediction sets
xyzu_train = xyzu[1:13897, ]
xyzu_pred = xyzu[13898:nrow(xyzu), ]

# Generate random initial points between 1000
nb_trials = 500              # number of different trials/initial prediction points
nb_series = 3                # number of univariate time series
set.seed(123)
initial_points = sort(sample(1:1000, nb_trials), decreasing = FALSE)
