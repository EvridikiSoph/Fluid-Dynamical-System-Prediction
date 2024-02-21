##################################
### Tuning of model parameters ###
##################################
# Generate dataframe with all possible inputs
gs <- list(ny = c(2, 3),
           nu = c(2, 3),
           nl = c(2, 3),
           rho = c(0.0000000001, 0.000000001, 0.00000001, 0.0000001, 0.000001))  %>% 
  cross_df() # Convert to data frame grid

# Fit model with different parameters
MSEs = rep(0, nrow(gs))

# Calculate MSE between prediction and real value for different combinations
for (i in 1:nrow(gs)) {
  
  MSEs_aux = rep(0, 5)
  k = 0
  
  # Iterate through the five different segments for crossvalidation
  for (l in 1:5) {
  
    # Create training and test sets
    train_set = as.matrix(xyzu_train[1:(9200+k), ])
    val_set = as.matrix(xyzu_train[(9200+k+1):(9200+k+100), ])

    # Train model
    model_narx = narx(ny = gs[i, 1]$ny, nu = gs[i, 2]$nu, nl = gs[i, 3]$nl)
    tryCatch({
      model_narx = estimate.narx(model_narx, train_set[, 1:3], as.matrix(train_set[, 4]), gs[i, 4]$rho)
    
      # Create predictions
      pred_narx = predict(model_narx, val_set[, 1:3], as.matrix(val_set[, 4]), K=0)
      MSE_sum = 0
    
      # Calculate MSE for each prediction and add results for all components
      for(j in 1:nb_series) {
        MSE = mean((val_set[-(1:max(gs[i, 1]$ny, gs[i, 2]$nu)), j] - pred_narx[[j]]$yh)^2)
        MSE_sum = MSE_sum + MSE
      }
    
      # Save to matrix
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
# Get optimal parameters
opt_n = data.frame(ny=2, nu=3, nl=3, rho=1e-9)

# Tuning smoothing parameters using PLR
MSEs_aux_n005 = rep(0, nb_series)
MSEs_aux_n01 = rep(0, nb_series)
MSEs_aux_n02 = rep(0, nb_series)

# Create training and test sets
train_set = as.matrix(xyzu_train[1:13594, ])
val_set = as.matrix(xyzu_train[13595:nrow(xyzu_train), ])

# Train model
narx_model = narx(opt_n$ny, opt_n$nu, opt_n$nl)
narx_model = estimate.narx(narx_model, train_set[, 1:3], as.matrix(train_set[, 4]), opt_n$rho)


##################
### 0.5% noise ###
##################
# Add 0.5% of noise to initial condition
val_set_n005 = val_set

for(j in 1:nb_series) {
  for(f in 1:opt_n$ny) {
    val_set_n005[f, j] = val_set_n005[f, j] + 0.005*sd(val_set[, j])
  }
}

# Create predictions
pred_n005_aux = predict.narx(narx_model, val_set_n005[, 1:3], as.matrix(val_set_n005[, 4]), K=0)

for(j in 1:nb_series) {
  
  pred = data.frame(pred = pred_n005_aux[[j]]$yh, time = seq(0.001, 0.303, 0.001))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")

  MSEs_aux_n005[j] = aux$h.opt[2, ]
}

    
################
### 1% noise ###
################
# Add 1% of noise to initial condition
val_set_n01 = val_set
    
for(j in 1:nb_series) {
  for(f in 1:opt_n$ny) {
    val_set_n01[f, j] = val_set_n01[f, j] + 0.01*sd(val_set[, j])
  }
}
    
# Create predictions
pred_n01_aux = predict.narx(narx_model, val_set_n01[, 1:3], as.matrix(val_set_n01[, 4]), K=0)
    
for(j in 1:nb_series) {
  
  pred = data.frame(pred = pred_n01_aux[[j]]$yh, time = seq(0.001, 0.303, 0.001))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")
  
  MSEs_aux_n01[j] = aux$h.opt[2, ]
}

    
################
### 2% noise ###
################
# Add 2% of noise to initial condition
val_set_n02 = val_set
    
for(j in 1:nb_series) {
  for(f in 1:opt_n$ny) {
    val_set_n02[f, j] = val_set_n02[f, j] + 0.02*sd(val_set[, j])
  }
}

# Create predictions
pred_n02_aux = predict.narx(narx_model, val_set_n02[, 1:3], as.matrix(val_set_n02[, 4]), K=0)
    
for(j in 1:nb_series) {
  
  pred = data.frame(pred = pred_n02_aux[[j]]$yh, time = seq(0.001, 0.303, 0.001))
  
  aux = np.cv(data = as.matrix(pred), h.seq = NULL, num.h = 1000, w = NULL, num.ln = 1,
              ln.0 = 0, step.ln = 2, estimator = "LLP", kernel = "triweight")

  MSEs_aux_n02[j] = aux$h.opt[2, ]
}
