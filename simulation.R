########################## Setting seed ##########################

set.seed(1234)


########################## Simulation 1 ##########################

# defining variables
sim1_dd <- NULL 
sim1_dd_bias_mean <- numeric(1000)
sim1_dd_bias_sd <- numeric(1000)
sim1_ema <- NULL 
sim1_ema_bias_mean <- numeric(1000)
sim1_ema_bias_sd <- numeric(1000)
rep <- 1
count <- c(1:100) 
day <- c(0:99) 

repeat {
  
  # simulating original time series
  sim1_ts <- rnorm(2400, mean = 10, sd = 1)
  
  # sampling of original time series
  for (i in count) {
    sim1_dd <- c(sim1_dd, mean(sim1_ts[c(1:24) + (day[i] * 24)]))
    sim1_ema <- c(sim1_ema, sim1_ts[9 + day[i] * 24], sim1_ts[13 + day[i] * 24], sim1_ts[18 + day[i] * 24])
  }
  
  # bias for EMA and DD
  sim1_dd_bias_mean[rep] <- abs(mean(sim1_dd) - mean(sim1_ts))
  sim1_dd_bias_sd[rep] <- abs(sd(sim1_dd) - sd(sim1_ts))
  sim1_ema_bias_mean[rep] <- abs(mean(sim1_ts) - mean(sim1_ema))
  sim1_ema_bias_sd[rep] <- abs(sd(sim1_ts) - sd(sim1_ema))
  
  # reset for next iteration
  sim1_dd <- NULL
  sim1_ema <- NULL
  
  if (rep > 999) {
    break
  }
  rep <- rep + 1
}


########################## Simulation 2 ##########################

# defining variables
sim2_dd <- NULL
sim2_dd_bias_mean <- numeric(1000)
sim2_dd_bias_sd <- numeric(1000)
sim2_ema <- NULL
sim2_ema_bias_mean <- numeric(1000)
sim2_ema_bias_sd <- numeric(1000)
rep <- 1
count <- c(1:100) 
day <- c(0:99) 

repeat {
  
  # simulating original time series
  sim2_ts <- arima.sim(list(ar = 0.9), n = 2400, mean = 10)
  
  for (i in count) {
    # sampling of original time series
    sim2_dd <- c(sim2_dd, mean(sim2_ts[c(1:24) + (day[i] * 24)]))
    sim2_ema <- c(sim2_ema, sim2_ts[9 + day[i] * 24], sim2_ts[13 + day[i] * 24], sim2_ts[18 + day[i] * 24])
  }
  
  # bias for EMA and DD
  sim2_dd_bias_mean[rep] <- abs(mean(sim2_ts) - mean(sim2_dd))
  sim2_dd_bias_sd[rep] <- abs(sd(sim2_ts) - sd(sim2_dd))
  sim2_ema_bias_mean[rep] <- abs(mean(sim2_ts) - mean(sim2_ema))
  sim2_ema_bias_sd[rep] <- abs(sd(sim2_ts) - sd(sim2_ema))
  
  # reset for next iteration
  sim2_dd <- NULL
  sim2_ema <- NULL
  
  if (rep > 999) {
    break
  }
  rep <- rep + 1
}


########################## Simulation 3 ##########################

# defining variables
sim3_dd <- NULL
sim3_ema <- NULL
sim3_dd_bias_tr <- numeric(1000)
sim3_ema_bias_tr <- numeric(1000)
rep <- 1
count <- c(1:100) 
day <- c(0:99) 

repeat {
  
  # simulating original time series
  sim3_ts <- rnorm(2400, mean = 10, sd = 1) + seq(1:2400) * 0.00015
  
  for (i in count) {
    
    # sampling of original time series
    sim3_dd <- c(sim3_dd, mean(sim3_ts[c(1:24) + (day[i] * 24)]))
    sim3_ema <- c(sim3_ema, sim3_ts[9 + day[i] * 24], sim3_ts[13 + day[i] * 24], sim3_ts[18 + day[i] * 24])
  }
  
  # bias for EMA and DD
  sim3_dd_bias_tr[rep] <- as.numeric(coef(summary(lm(sim3_dd ~ c(1:100))))[, "t value"][2])
  sim3_ema_bias_tr[rep] <- as.numeric(coef(summary(lm(sim3_ema ~ c(1:300))))[, "t value"][2])

  # reset for next iteration
  sim3_dd <- NULL
  sim3_ema <- NULL
  
  if (rep > 999) {
    break
  }
  rep <- rep + 1
}


########################## Simulation 4 ##########################

# defining variables
sim4_dd <- NULL
sim4_ema <- NULL
sim4_dd_bias <- numeric(1000)
sim4_ema_bias <- numeric(1000)
sim4_ts_events <- numeric(1000)
rep <- 1
count <- c(1:100) 
day <- c(0:99) 

repeat {
  
  # simulating original time series
  sim4_ts <- rbeta(2400, shape1 = 0.01, shape2 = 10)
  
  for (i in count) {
    # sampling of original time series
    sim4_dd <- c(sim4_dd, mean(sim4_ts[c(1:24) + (day[i] * 24)]))
    sim4_ema <- c(sim4_ema, sim4_ts[9 + day[i] * 24], sim4_ts[13 + day[i] * 24], sim4_ts[18 + day[i] * 24])
  }
  
  # bias for EMA and DD
  sim4_ts_events[rep] <- sum(sim4_ts > (mean(sim4_ts) + sd(sim4_ts)))
  sim4_dd_bias[rep] <- sum(sim4_dd > (mean(sim4_dd) + sd(sim4_dd)))
  sim4_ema_bias[rep] <- sum(sim4_ema > (mean(sim4_ema) + sd(sim4_ema)))

  # reset for next iteration
  sim4_dd <- NULL
  sim4_ema <- NULL
  
  if (rep > 999) {
    break
  }
  rep <- rep + 1
}


############################## Plots ##############################

# simulation 1
sim1_plot_dd <- NULL
sim1_plot_ema <- NULL
sim1_plot_ts <- rnorm(2400, mean = 10, sd = 1)

for (i in count) {
  sim1_plot_dd <- c(sim1_plot_dd, rep(mean(sim1_plot_ts[c(1:24) + (day[i] * 24)]),24))
}

for (i in count) {
  sim1_plot_ema <- c(sim1_plot_ema, c(rep(NA,8),sim1_plot_ts[9 + day[i] * 24], rep(NA,3),sim1_plot_ts[13 + day[i] * 24], rep(NA,4),sim1_plot_ts[18 + day[i] * 24],rep(NA,6)))
}

plot(c(1:2400),sim1_plot_ts,type="l",col="grey", xaxt='n',yaxt='n', ann=FALSE)
lines(c(1:2400),sim1_plot_dd,type="l",col="blue")
lines(c(1:2400),sim1_plot_ema,col="red",type="p",pch="*")

# simulation 2
sim2_plot_dd <- NULL
sim2_plot_ema <- NULL
sim2_plot_ts <- arima.sim(list(ar = 0.9), n = 2400, mean = 10)

for (i in count) {
  sim2_plot_dd <- c(sim2_plot_dd, rep(mean(sim2_plot_ts[c(1:24) + (day[i] * 24)]),24))
}

for (i in count) {
  sim2_plot_ema <- c(sim2_plot_ema, c(rep(NA,8),sim2_plot_ts[9 + day[i] * 24], rep(NA,3),sim2_plot_ts[13 + day[i] * 24], rep(NA,4),sim2_plot_ts[18 + day[i] * 24],rep(NA,6)))
}

plot(c(1:2400),sim2_plot_ts,type="l",col="grey", xaxt='n',yaxt='n', ann=FALSE)
lines(c(1:2400),sim2_plot_dd,type="l",col="blue")
lines(c(1:2400),sim2_plot_ema,col="red",type="p",pch="*")

# simulation 3
sim3_plot_dd <- NULL
sim3_plot_ema <- NULL
sim3_plot_ts <- rnorm(2400, mean = 10, sd = 1) + seq(1:2400) * 0.00015

for (i in count) {
  sim3_plot_dd <- c(sim3_plot_dd, rep(mean(sim3_plot_ts[c(1:24) + (day[i] * 24)]),24))
}

for (i in count) {sim3_plot_ema <- c(sim3_plot_ema, c(rep(NA,8),sim3_plot_ts[9 + day[i] * 24], rep(NA,3),sim3_plot_ts[13 + day[i] * 24], rep(NA,4),sim3_plot_ts[18 + day[i] * 24],rep(NA,6)))
}

plot(c(1:2400),sim3_plot_ts,type="l",col="grey", xaxt='n',yaxt='n', ann=FALSE)
lines(c(1:2400),sim3_plot_dd,type="l",col="blue")
lines(c(1:2400),sim3_plot_ema,col="red",type="p",pch="*")

# simulation 4
sim4_plot_dd <- NULL
sim4_plot_ema <- NULL
sim4_plot_ts <- rbeta(2400, shape1 = 0.01, shape2 = 10)

for (i in count) {
  sim4_plot_dd <- c(sim4_plot_dd, rep(mean(sim4_plot_ts[c(1:24) + (day[i] * 24)]),24))
}

for (i in count) {
  sim4_plot_ema <- c(sim4_plot_ema, c(rep(NA,8),sim4_plot_ts[9 + day[i] * 24], rep(NA,3),sim4_plot_ts[13 + day[i] * 24], rep(NA,4),sim4_plot_ts[18 + day[i] * 24],rep(NA,6)))
}

plot(c(1:2400),sim4_plot_ts,type="l",col="grey", xaxt='n',yaxt='n', ann=FALSE)
lines(c(1:2400),sim4_plot_ema,col="red",type="p",pch="*")
lines(c(1:2400),sim4_plot_dd,type="l",col="blue")


