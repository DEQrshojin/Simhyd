for (i in 1) {
  # 1) Set parameters
  # Read in the csvs
  rPath <- 'C:/Users/ryans/Desktop/001_RMS/002_projects/scripts/simhyd_v2/'
  datPath <- paste0(rPath, 'data_test_one/')
  parTxt <- read.csv(paste0(datPath, 'par.csv'))
  rteTxt <- read.csv(paste0(datPath, 'rte.csv'))
  clbTxt <- read.csv(paste0(datPath, 'calib.csv'))
  run = clbTxt[nrow(clbTxt), 1] + 1
  # Set the parameters
  parTxt[1, 5] <- 0.023 # ****GOOD**** Baseflow
  parTxt[2, 5] <- 400.0 # ****GOOD**** Infiltration coefficient
  parTxt[3, 5] <- 0.750 # ****GOOD**** Infitration shape
  parTxt[4, 5] <- 0.090 # ****GOOD**** Interflow coefficient
  parTxt[5, 5] <- 0.400 # ****GOOD**** Recharge coefficient
  parTxt[6, 5] <- 5.000 # ****GOOD**** Rainfall interception
  parTxt[7, 5] <- 50.00 # ****GOOD**** Soil moisture storage
  parTxt[8, 5] <- 1.000 # ****GOOD**** Pervious fraction
  parTxt[9, 5] <- 0.000 # ****GOOD**** Impervious threshold
  rteTxt[1, 2] <- 0.000 # Turned off Routing delay
  rteTxt[1, 3] <- 0.100 # Turned off Inflow/outflow bias
  # Check the conditions on the routing parameters x < 0.5 Dt/k < 1 - x
  intMsk = 0.5 / rteTxt[1, 2]
  if(rteTxt[1, 3] < intMsk & intMsk < (1 - rteTxt[1, 3])) {
    print("Rounting condition met")
  } else {
    print("Rounting condition violated")
  }
  # 2) Write parameters to input files
  write.csv(parTxt, file = paste0(datPath, 'par.csv'), row.names = FALSE)
  write.csv(rteTxt, file = paste0(datPath, 'rte.csv'), row.names = FALSE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) RUN MODEL
# 4) RUN CALIBRATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in 1) {
  # 5) Write parameters and calibration results to file
  clbTxt[run, 1] = run
  clbTxt[run, 2 : 10] = parTxt[1 : 9, 5]
  clbTxt[run, 11 : 12] = rteTxt[1, 2 : 3]
  for (i in 1 : length(calStt)) {
    clbTxt[run, i + 12] = calStt[[i]]
  }
  write.csv(clbTxt, file = paste0(datPath, 'calib.csv'), row.names = FALSE)
}

