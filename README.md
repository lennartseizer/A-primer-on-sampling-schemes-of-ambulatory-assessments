# A-primer-on-sampling-schemes-of-ambulatory-assessments

This code compares the performance of two time series sampling methods, Daily Diary (DD) and Ecological Momentary Assessment (EMA), across four different simulations. Each simulation generates a time series, applies both sampling methods, calculates biases to evaluate their performance, and generates plots for visual comparison.

## Simulations

Simulation 1: 
Generates a time series using random normally distributed values and calculates the mean and standard deviation biases for both sampling methods over 1000 iterations.

Simulation 2: 
Generates a time series using an autoregressive model with a lag of 1 and calculates the mean and standard deviation biases for both sampling methods over 1000 iterations.

Simulation 3: 
Generates a time series using random normally distributed values with a linear trend and calculates the trend bias for both sampling methods over 1000 iterations.

Simulation 4: 
Generates a time series using random beta distributed values and calculates the bias of extreme values (values above the mean + standard deviation) for both sampling methods over 1000 iterations.

## Plots

The script generates four plots, one for each simulation. The plots show the original time series in grey, the DD sampled time series in blue, and the EMA sampled time series in red with asterisk markers.

