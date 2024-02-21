# Define optimal parameters
opt_n = data.frame(degree=3, lambda=0.05)

# Initiate lists to save predictions in
preds_nn = list()
preds_n005 = list()
preds_n01 = list()
preds_n02 = list()

# Create predictions for n different initial points
for (i in 1:nb_trials) {
  
  print(i)
  
  # Create training set
  if (initial_points[i] == 1) {
    train_set = as.matrix(xyzu_train)
  }
  
  if (initial_points[i] > 1) {
    train_set = as.matrix(rbind(xyzu_train, xyzu_pred[1:(initial_points[i]-1), ]))
  }
  
  # Create prediction set
  pred_set = as.matrix(xyzu_pred[initial_points[i]:(initial_points[i]+99), ])
  x0 = pred_set[1, 1:3]                # initial point
  u = pred_set[, 4]
  steps = 50                         # number of time steps to take
  
  # Create training feature matrix
  feat_mat_train = sindyr::features(train_set, polyorder = opt_n$degree)
  
  # Train model
  sindy_model = sindy(train_set, dt = dt, Theta = feat_mat_train, lambda = opt_n$lambda)
  coeff = data.frame(sindy_model$B)
  
  
  ################
  ### No noise ###
  ################
  # Create prediction an real values
  xhat_nn = matrix(0, steps, nb_series)
  xhat_nn[1, ] = x0  
  
  for (j in 1:(steps-1)) {
    xhat_nn[j+1, ] = rk4u(sindy_pred, xhat_nn[j, ], u[j], dt, coeff, opt_n$degree)
  }
  
  # Creation of final dataframes
  pred_nn_aux = list()
  
  for(k in 1:nb_series) {
    pred_nn_aux[[k]] = data.frame(cbind(xhat_nn[, k], pred_set[, k]))
    colnames(pred_nn_aux[[k]]) = c('yh', 'Y')
  }
  preds_nn[[i]] = pred_nn_aux
  
  
  ##################
  ### 0.5% noise ###
  ##################
  # Add 0.5% of noise to initial point
  x0n005 = rep(0, length(x0))
  
  for (j in 1:nb_series) {
    x0n005[j] = x0[j] + 0.005*sd(pred_set[, j])
  }
  
  # Create prediction an real values
  xhat_n005 = matrix(0, steps, nb_series)
  xhat_n005[1, ] = x0n005 
  
  for (j in 1:(steps-1)) {
    xhat_n005[j+1, ] = rk4u(sindy_pred, xhat_n005[j, ], u[j], dt, coeff, opt_n$degree)
  }
  
  # Creation of final dataframes
  pred_n005_aux = list()
  
  for(k in 1:nb_series) {
    pred_n005_aux[[k]] = data.frame(cbind(xhat_n005[, k], pred_set[, k]))
    colnames(pred_n005_aux[[k]]) = c('yh', 'Y')
  }
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = xhat_n005[, j], time = seq(0.001, 0.1, 0.001))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = 0.008, newt = NULL, estimator = "LLP", kernel = "triweight")
    pred_n005_aux[[j]]$pred_smooth = smoothed_pred
  }
  
  preds_n005[[i]] = pred_n005_aux
  
  
  ################
  ### 1% noise ###
  ################
  # Add 1% of noise to initial point
  x0n01 = rep(0, length(x0))
  
  for (j in 1:nb_series) {
    x0n01[j] = x0[j] + 0.01*sd(pred_set[, j])
  }
  
  # Create prediction an real values
  xhat_n01 = matrix(0, steps, nb_series)
  xhat_n01[1, ] = x0n01 
  
  for (j in 1:(steps-1)) {
    xhat_n01[j+1, ] = rk4u(sindy_pred, xhat_n01[j, ], u[j], dt, coeff, opt_n$degree)
  }
  
  # Creation of final dataframes
  pred_n01_aux = list()
  
  for(k in 1:nb_series) {
    pred_n01_aux[[k]] = data.frame(cbind(xhat_n01[, k], pred_set[, k]))
    colnames(pred_n01_aux[[k]]) = c('yh', 'Y')
  }
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = xhat_n01[, j], time = seq(0.001, 0.1, 0.001))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = 0.008, newt = NULL, estimator = "LLP", kernel = "triweight")
    pred_n01_aux[[j]]$pred_smooth = smoothed_pred
  }
  
  preds_n01[[i]] = pred_n01_aux

    
  ################
  ### 2% noise ###
  ################
  # Add 2% of noise to initial point
  x0n02 = rep(0, length(x0))
  
  for (j in 1:nb_series) {
    x0n02[j] = x0[j] + 0.02*sd(pred_set[, j])
  }
  
  # Create prediction an real values
  xhat_n02 = matrix(0, steps, nb_series)
  xhat_n02[1, ] = x0n02 
  
  for (j in 1:(steps-1)) {
    xhat_n02[j+1, ] = rk4u(sindy_pred, xhat_n02[j, ], u[j], dt, coeff, opt_n$degree)
  }
  
  # Creation of final dataframes
  pred_n02_aux = list()
  
  for(k in 1:nb_series) {
    pred_n02_aux[[k]] = data.frame(cbind(xhat_n02[, k], pred_set[, k]))
    colnames(pred_n02_aux[[k]]) = c('yh', 'Y')
  }
  
  # Smoothing the prediction
  for(j in 1:nb_series) {
    pred = data.frame(pred = xhat_n02[, j], time = seq(0.001, 0.1, 0.001))
    smoothed_pred = np.est(data = as.matrix(pred), h.seq = 0.008, newt = NULL, estimator = "LLP", kernel = "triweight")
    pred_n02_aux[[j]]$pred_smooth = smoothed_pred
  }
  
  preds_n02[[i]] = pred_n02_aux
}


#######################################
### Save prediction and model lists ###
#######################################
saveRDS(preds_nn, file="Results/sindy_preds_nn.RData")
saveRDS(preds_n005, file="Results/sindy_preds_n005.RData")
saveRDS(preds_n01, file="Results/sindy_preds_n01.RData")
saveRDS(preds_n02, file="Results/sindy_preds_n02.RData")
