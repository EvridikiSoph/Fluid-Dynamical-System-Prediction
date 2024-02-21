#####################################
### Read in predictions and model ###
#####################################
preds_nn = readRDS("Results/narx_preds_nn.RData")
preds_n005 = readRDS("Results/narx_preds_n005.RData")
preds_n01 = readRDS("Results/narx_preds_n01.RData")
preds_n02 = readRDS("Results/narx_preds_n02.RData")
nb_time_steps = 50
coordinates = c('x', 'y', "z")


#################################
### Plots of no noise results ###
#################################
# Calculate NRMSE distribution per coordinate
NRMSEs_nn = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    obs_sd = sd(preds_nn[[i]][[j]]$Y[1:nb_time_steps])
    NRMSE = sqrt(mean((preds_nn[[i]][[j]]$yh[1:nb_time_steps] - preds_nn[[i]][[j]]$Y[1:nb_time_steps])^2))/obs_sd
    NRMSEs_nn[i, j] = NRMSE
  }
}

NRMSEs_nn = data.frame(NRMSEs_nn)
NRMSEs_nn$index = 1:nb_trials
names(NRMSEs_nn) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
means_nn = apply(NRMSEs_nn, 2, mean) 
sd_nn = apply(NRMSEs_nn, 2, sd)   

# Calculate average NRMSE and for each time step of each 
NRMSEs_nn_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_nn_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_nn_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {

  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = preds_nn[[k]][[j]]$Y[i] 
      pred_aux[k, 2] = preds_nn[[k]][[j]]$yh[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_sd = sd(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_sd
    NRMSEs_nn_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "bca")
    NRMSEs_nn_timestep_minCI[i, j] = CI$bca[4]
    NRMSEs_nn_timestep_maxCI[i, j] = CI$bca[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_nn[[1]][[j]]$Y[1:nb_time_steps], 
                       Label = "Real")
  df_pred = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_nn[[1]][[j]]$yh[1:nb_time_steps], 
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


######################################
### Plots of results of 0.5% noise ###
######################################
# Calculate NRMSE distributions per coordinate
NRMSEs_n005 = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    obs_sd = sd(preds_n005[[i]][[j]]$Y[1:nb_time_steps])
    NRMSE = sqrt(mean((preds_n005[[i]][[j]]$yh[1:nb_time_steps] - preds_n005[[i]][[j]]$Y[1:nb_time_steps])^2))/obs_sd
    NRMSEs_n005[i, j] = NRMSE
  }
}

NRMSEs_n005 = data.frame(NRMSEs_n005)
NRMSEs_n005$index = 1:nb_trials
names(NRMSEs_n005) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
means_n005 = apply(NRMSEs_n005, 2, mean) 
sd_n005 = apply(NRMSEs_n005, 2, sd)   

# Calculate average NRMSE and for each time step of each 
NRMSEs_n005_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_n005_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = preds_n005[[k]][[j]]$Y[i] 
      pred_aux[k, 2] = preds_n005[[k]][[j]]$yh[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_sd = sd(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_sd
    NRMSEs_n005_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "bca")
    NRMSEs_n005_timestep_minCI[i, j] = CI$bca[4]
    NRMSEs_n005_timestep_maxCI[i, j] = CI$bca[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_n005[[1]][[j]]$Y[1:nb_time_steps],
                       Label = "Real")
  df_pred = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_n005[[1]][[j]]$yh[1:nb_time_steps],
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


####################################
### Plots of results of 1% noise ###
####################################
# Calculate NRMSE distributions per coordinate
NRMSEs_n01 = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    obs_sd = sd(preds_n01[[i]][[j]]$Y[1:nb_time_steps])
    NRMSE = sqrt(mean((preds_n01[[i]][[j]]$yh[1:nb_time_steps] - preds_n01[[i]][[j]]$Y[1:nb_time_steps])^2))/obs_sd
    NRMSEs_n01[i, j] = NRMSE
  }
}

NRMSEs_n01 = data.frame(NRMSEs_n01)
NRMSEs_n01$index = 1:nb_trials
names(NRMSEs_n01) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
means_n01 = apply(NRMSEs_n01, 2, mean)
sd_n01 = apply(NRMSEs_n01, 2, sd)   

# Calculate average NRMSE and for each time step of each 
NRMSEs_n01_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_n01_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = preds_n01[[k]][[j]]$Y[i] 
      pred_aux[k, 2] = preds_n01[[k]][[j]]$yh[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_sd = sd(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2
    
    NRMSE = sqrt(mean(pred_errors))/obs_sd
    NRMSEs_n01_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "bca")
    NRMSEs_n01_timestep_minCI[i, j] = CI$bca[4]
    NRMSEs_n01_timestep_maxCI[i, j] = CI$bca[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_n01[[1]][[j]]$Y[1:nb_time_steps],
                       Label = "Real")
  df_pred = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_n01[[1]][[j]]$yh[1:nb_time_steps],
                       Label = "Predicted")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title=element_text(size=30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title=element_text(size=30,face="bold"),
                legend.text=element_text(size=25)))
}


######################################
### Plots of results with 2% noise ###
######################################
# Calculate NRMSE distributions per coordinate
NRMSEs_n02 = matrix(0, nb_trials, nb_series)

for(i in 1:nb_trials) {
  for(j in 1:nb_series) {
    obs_sd = sd(preds_n02[[i]][[j]]$Y[1:nb_time_steps])
    NRMSE = sqrt(mean((preds_n02[[i]][[j]]$yh[1:nb_time_steps] - preds_n02[[i]][[j]]$Y[1:nb_time_steps])^2))/obs_sd
    NRMSEs_n02[i, j] = NRMSE
  }
}

NRMSEs_n02 = data.frame(NRMSEs_n02)
NRMSEs_n02$index = 1:nb_trials
names(NRMSEs_n02) = c('x', 'y', 'z', 'index')

# Calculation of max, min, mean, var
means_n02 = apply(NRMSEs_n02, 2, mean)
sd_n02 = apply(NRMSEs_n02, 2, sd)

# Calculate average NRMSE and for each time step of each 
NRMSEs_n02_timestep = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_minCI = matrix(0, nb_time_steps, nb_series)
NRMSEs_n02_timestep_maxCI = matrix(0, nb_time_steps, nb_series)

# Calculate NRMSE and CI for each time series component
for(j in 1:nb_series) {
  
  # Calculate prediction error for each time step for all trials
  for(i in 1:nb_time_steps) {
    pred_aux = matrix(0, nb_trials, 2)
    for(k in 1:nb_trials) {
      pred_aux[k, 1] = preds_n02[[k]][[j]]$Y[i] 
      pred_aux[k, 2] = preds_n02[[k]][[j]]$yh[i]
    }
    
    # Calculate NRMSE for each timestep
    obs_sd = sd(pred_aux[, 1])
    pred_errors = (pred_aux[, 1] - pred_aux[, 2])^2

    NRMSE = sqrt(mean(pred_errors))/obs_sd
    NRMSEs_n02_timestep[i, j] = NRMSE
    
    # Apply non parametric bootstrap of NRMSE statistic
    boot_out = boot::boot(pred_aux, R = 1000, statistic = NRMSE_function)
    CI = boot.ci(boot_out, type = "bca")
    NRMSEs_n02_timestep_minCI[i, j] = CI$bca[4]
    NRMSEs_n02_timestep_maxCI[i, j] = CI$bca[5]
  }
}

# Plot results of complete dataset and prediction
for(j in 1:(nb_series)) {
  
  df_real = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_n02[[1]][[j]]$Y[1:nb_time_steps], 
                       Label = "Real")
  df_pred = data.frame(time = seq(1,nb_time_steps,length=nb_time_steps), Coordinate = preds_n02[[1]][[j]]$yh[1:nb_time_steps], 
                       Label = "Predicted Smooth")
  
  df = rbind(df_real, df_pred)
  
  print(ggplot(df, aes(x = time, y = Coordinate, color = Label)) + 
          geom_line(size=1.5) +
          theme_minimal() +
          theme(plot.title = element_text(size = 30,face="bold"), 
                axis.text=element_text(size=25),
                axis.title=element_text(size=30,face="bold"), 
                legend.title = element_text(size = 30,face="bold"),
                legend.text = element_text(size=25)))
}


#################################
### Plots for each coordinate ###
#################################
for(i in 1:nb_series) {
  
  # Create dataframe with all NRMSE values per trial of coordinate
  NRMSEs_x = data.frame(index = 1:nb_trials, NRMSEs_nn[, i], 
                                             NRMSEs_n005[, i],
                                             NRMSEs_n01[, i], 
                                             NRMSEs_n02[, i])
  
  names(NRMSEs_x) = c('Index', '0% noise', '0.5% noise', '1% noise', '2% noise')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_long = gather(NRMSEs_x, key="Noise", value="NRMSE", 2:5)

  # Add vector to order noise properly
  order_vec = c(rep(4, 500), rep(3, 500), rep(2, 500), rep(1, 500))
  NRMSEs_x_long = data.frame(NRMSEs_x_long, order_vec)
  
  print(ggplot(NRMSEs_x_long, aes(x = log(NRMSE), fill = Noise, colour = Noise)) +
          theme_minimal() +
          theme(plot.title = element_text(size = 50,face="bold"), 
                axis.text=element_text(size=40),
                axis.title=element_text(size=45,face="bold"), 
                legend.text = element_text(size=40),
                legend.title = element_text(size = 45,face="bold")) +
          xlab("NRMSE") + ylab("Density") +
          geom_density(alpha = 1.5, aes(color=Noise, fill = Noise)) +
          stat_density(adjust = 1.5))

  # Prepare NRMSE
  # Create a dataframe with all NRMSE values per time step of coordinate 
  NRMSEs_x_timestep = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep[, i], 
                                                          NRMSEs_n005_timestep[, i], 
                                                          NRMSEs_n01_timestep[, i], 
                                                          NRMSEs_n02_timestep[, i])
  
  names(NRMSEs_x_timestep) = c('Timestep', '0%', '0.5%', '1%', '2%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long = gather(NRMSEs_x_timestep, key="Noise", value="NRMSE", 2:5)

  # Prepare NRMSE min CI
  # Create a dataframe with all min CIs of the NRMSE values per time step of coordinate 
  NRMSEs_x_timestep_minCI = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep_minCI[, i], 
                                                                NRMSEs_n005_timestep_minCI[, i], 
                                                                NRMSEs_n01_timestep_minCI[, i], 
                                                                NRMSEs_n02_timestep_minCI[, i])
  
  names(NRMSEs_x_timestep_minCI) = c('Timestep', '0%', '0.5%', '1%', '2%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long_minCI = gather(NRMSEs_x_timestep_minCI, key="Noise", value="min_CI", 2:5)
  
  # Prepare NRMSE max CI
  # Create a dataframe with all max CIs of the NRMSE values per time step of coordinate 
  NRMSEs_x_timestep_maxCI = data.frame(index = 1:nb_time_steps, NRMSEs_nn_timestep_maxCI[, i], 
                                                                NRMSEs_n005_timestep_maxCI[, i], 
                                                                NRMSEs_n01_timestep_maxCI[, i], 
                                                                NRMSEs_n02_timestep_maxCI[, i])
  
  names(NRMSEs_x_timestep_maxCI) = c('Timestep', '0%', '0.5%', '1%', '2%')
  
  # Concatinate all NRMSE values for all levels of noise into a long df
  NRMSEs_x_timestep_long_maxCI = gather(NRMSEs_x_timestep_maxCI, key="Noise", value="max_CI", 2:5)

  df_joined = data.frame(NRMSEs_x_timestep_long, 
                        "min_CI" = NRMSEs_x_timestep_long_minCI[, 3],
                        "max_CI" = NRMSEs_x_timestep_long_maxCI[, 3])
  
  df_joined$Noise  <- with(df_joined, reorder(Noise, NRMSE))
  
  # Plot the evolution of NRMSE over time
  print(ggplot(df_joined, aes(x = Timestep, y = NRMSE, group = Noise)) +
          theme_minimal() +
          theme(plot.title = element_text(size = 50,face="bold"), 
                axis.text=element_text(size=40),
                axis.title=element_text(size=45,face="bold"), 
                legend.text = element_text(size=40),
                legend.title = element_text(size = 45,face="bold")) +
          geom_line(aes(color=Noise), size = 1.7) +
          geom_point(aes(color=Noise), size = 1.7) +
          geom_ribbon(aes(y = NRMSE, ymin = min_CI, ymax = max_CI, fill = Noise), alpha = .2))
}


######################################
### Tests to compare distributions ###
######################################
# Kolmogorov-Smirnov test
ks.test(NRMSEs_nn$x, NRMSEs_n005$x)
ks.test(NRMSEs_nn$x, NRMSEs_n01$x)
ks.test(NRMSEs_nn$x, NRMSEs_n02$x)

ks.test(NRMSEs_n005$x, NRMSEs_n01$x)
ks.test(NRMSEs_n005$x, NRMSEs_n02$x)

ks.test(NRMSEs_n01$x, NRMSEs_n02$x)


ks.test(NRMSEs_nn$y, NRMSEs_n005$y)
ks.test(NRMSEs_nn$y, NRMSEs_n01$y)
ks.test(NRMSEs_nn$y, NRMSEs_n02$y)

ks.test(NRMSEs_n005$y, NRMSEs_n01$y)
ks.test(NRMSEs_n005$y, NRMSEs_n02$y)

ks.test(NRMSEs_n01$y, NRMSEs_n02$y)


ks.test(NRMSEs_nn$z, NRMSEs_n005$z)
ks.test(NRMSEs_nn$z, NRMSEs_n01$z)
ks.test(NRMSEs_nn$z, NRMSEs_n02$z)

ks.test(NRMSEs_n005$z, NRMSEs_n01$z)
ks.test(NRMSEs_n005$z, NRMSEs_n02$z)

ks.test(NRMSEs_n01$z, NRMSEs_n02$z)
