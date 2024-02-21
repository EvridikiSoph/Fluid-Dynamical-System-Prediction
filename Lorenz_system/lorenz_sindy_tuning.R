##################################
### Tuning of model parameters ###
##################################
# Potential lambdas and polynomial degrees
gs <- list(degree = c(2, 3),
           lambda = seq(0.001, 0.05, 0.0005)) %>%
  cross_df() # Convert to data frame grid

# Fit model with different parameters
MSEs = rep(0, nrow(gs))

# Calculate MSE between prediction and real value for different combinations
for (i in 1:nrow(gs)) {
  
  print(i)
  MSEs_aux = rep(0, 5)
  k = 0
  
  # Iterate through the five different segments for crossvalidation
  for (l in 1:5) {
    
    # Create training and test sets
    train_set = as.matrix(xyzu_train[1:(9200+k), ])
    val_set = as.matrix(xyzu_train[(9200+k+1):(9200+k+100), ])
    
    x0 = val_set[1, 1:3]                # initial point
    u = val_set[, 4]
    steps = nrow(val_set)
    col_names = c("x", "y", "z", "u")
    colnames(train_set) = col_names
    
    tryCatch({
      # Train model 
      feat_mat_train = sindyr::features(train_set, polyorder = gs$degree[i])
      snd = sindy(train_set, Theta = feat_mat_train, lambda = gs$lambda[i], dt = dt)
      coeff = data.frame(snd$B)
      
      # Create prediction an real values
      xhat = matrix(0, steps, nb_series)
      xhat[1, ] = x0  
      
      for (j in 1:(steps-1)) {
        xhat[j+1, ] = rk4u(sindy_pred, xhat[j, ], u[j], dt, coeff, gs$degree[i])
      }
        
      MSE_sum = 0
      
      for(j in 1:nb_series) {
        MSE = mean((val_set[, j] - xhat[, j])^2)
        MSE_sum = MSE_sum + MSE
      }
    
      MSEs_aux[l] = MSE_sum/nb_series
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
    
    # Take next step in crossvalidation
    k = k + 100
  }
  MSEs[i] = mean(MSEs_aux)
  print(MSEs[i])
}  


######################################
### Tuning of smoothing parameters ###
######################################
# Define optimal parameters
opt_n = data.frame(degree=3, lambda=0.05)

# Create training and test sets
train_set = as.matrix(xyzu_train[1:13597, ])
val_set = as.matrix(xyzu_train[13598:nrow(xyzu_train), ])
x0 = val_set[1, 1:3]                # initial point
u = val_set[, 4]
steps = 100                         # number of time steps to take

# Create training feature matrix
feat_mat_train = sindyr::features(train_set, polyorder = opt_n$degree)

# Train model
sindy_model = sindy(train_set, dt = dt, Theta = feat_mat_train, lambda = opt_n$lambda)
coeff = data.frame(sindy_model$B)


##################
### 0.5% noise ###
##################
# Add 0.5% of noise to initial point
x0n005 = rep(0, length(x0))

for (j in 1:nb_series) {
  x0n005[j] = x0[j] + 0.005*sd(val_set[, j])
}

# Create prediction an real values
xhat_n005 = matrix(0, steps, nb_series)
xhat_n005[1, ] = x0n005 

for (j in 1:(steps-1)) {
  xhat_n005[j+1, ] = rk4u(sindy_pred, xhat_n005[j, ], u[j], dt, coeff, opt_n$degree)
}

# Creation of final dataframes
pred_aux_n005 = list()

for(k in 1:nb_series) {
  pred_aux_n005[[k]] = cbind(xhat_n005[, k], val_set[, k])
  colnames(pred_aux_n005[[k]]) = c('pred', 'real')
}

# Leave one out crossvalidation
MSEs_aux_n005 = rep(0, 3)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = data.frame(pred_aux_n005[[j]])$pred, time = seq(0.001, 0.3, 0.001))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  
  MSEs_aux_n005[j] = aux$h.opt[2, ]
}


################
### 1% noise ###
################
# Add 1% of noise to initial condition
x0n01 = rep(0, length(x0))

for (j in 1:nb_series) {
  x0n01[j] = x0[j] + 0.01*sd(val_set[, j])
}

# Create prediction an real values
xhat_n01 = matrix(0, steps, nb_series)
xhat_n01[1, ] = x0n01 

for (j in 1:(steps-1)) {
  xhat_n01[j+1, ] = rk4u(sindy_pred, xhat_n01[j, ], u[j], dt, coeff, opt_n$degree)
}

# Creation of final dataframes
pred_aux_n01 = list()

for(k in 1:nb_series) {
  pred_aux_n01[[k]] = cbind(xhat_n01[, k], val_set[, k])
  colnames(pred_aux_n01[[k]]) = c('pred', 'real')
}

# Leave one out crossvalidation
MSEs_aux_n01 = rep(0, 3)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = data.frame(pred_aux_n01[[j]])$pred, time = seq(0.001, 0.3, 0.001))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  
  MSEs_aux_n01[j] = aux$h.opt[2, ]
}


################
### 2% noise ###
################
# Add 2% of noise to initial condition
x0n02 = rep(0, length(x0))

for (j in 1:nb_series) {
  x0n02[j] = x0[j] + 0.02*sd(val_set[, j])
}

# Create prediction an real values
xhat_n02 = matrix(0, steps, nb_series)
xhat_n02[1, ] = x0n02 

for (j in 1:(steps-1)) {
  xhat_n02[j+1, ] = rk4u(sindy_pred, xhat_n02[j, ], u[j], dt, coeff, opt_n$degree)
}

# Creation of final dataframes
pred_aux_n02 = list()

for(k in 1:nb_series) {
  pred_aux_n02[[k]] = cbind(xhat_n02[, k], val_set[, k])
  colnames(pred_aux_n02[[k]]) = c('pred', 'real')
}

# Leave one out crossvalidation
MSEs_aux_n02 = rep(0, 3)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = data.frame(pred_aux_n02[[j]])$pred, time = seq(0.001, 0.3, 0.001))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  
  MSEs_aux_n02[j] = aux$h.opt[2, ]
}
