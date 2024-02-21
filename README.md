# Fluid-Dynamical-System-Prediction

For time series (Lorenz and fluidic pinball) and each method (NARX, NARMAX, SINDy), there are four 
scripts:
- {time_series_name}_preprocessing: Includes all the time series preprocessing steps and needs to be 
run before any of the other scripts.
- {time_series_name}_{model_name}_functions: Includes all the functions necessary to apply the
model on the specific time series and needs to be run before the training, the application and the tuning 
file.
- {time_series_name}_{model_name}_tuning: Script that tunes the parameters of each model.
- {time_series_name}_{model_name}_application: Script that creates predictions for all levels of noise 
in the initial condition.
- {time_series_name}_{model_name}_plots: Script that creates diagnostic plots and statistics to 
evaluate the methods performance.
