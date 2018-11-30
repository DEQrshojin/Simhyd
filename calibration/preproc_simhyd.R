# 1) Set parameters
# Read in the csvs
# parTxt <- read.csv(paste0(datPath, 'par.csv'))
# rteTxt <- read.csv(paste0(datPath, 'rte.csv'))
# clbTxt <- read.csv(paste0(datPath, 'calib.csv'))
# 
# run = clbTxt[nrow(clbTxt), 1] + 1
# 
# # Set the parameters
# parTxt[1, 5] <- 0.03 # Baseflow 
# parTxt[2, 5] <- 200. # Infiltration coefficient
# parTxt[3, 5] <- 0.30 # Infitration shape 
# parTxt[4, 5] <- 0.50 # Interflow coefficient
# parTxt[5, 5] <- 0.10 # Recharge coefficient
# parTxt[6, 5] <- 0.00 # Rainfall interception
# parTxt[7, 5] <- 500. # Soil moisture storage
# parTxt[8, 5] <- 1.00 # Pervious fraction
# parTxt[9, 5] <- 0.00 # Impervious threshold
# rteTxt[1, 2] <- 0.00 # Routing delay
# rteTxt[1, 3] <- 0.25 # Inflow/outflow bias
# 
# # 2) Write parameters to file
# write.csv(parTxt, file = paste0(datPath, 'par.csv'), row.names = FALSE)
# write.csv(rteTxt, file = paste0(datPath, 'rte.csv'), row.names = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) RUN MODEL
# 4) RUN CALIBRATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 5) Write parameters and calibration results to file




