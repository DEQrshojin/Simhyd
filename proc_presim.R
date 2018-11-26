proc_presim <- function(inputs, qData, strDate, endDate) {
  
  str = as.numeric(Sys.time())
  
  for (i in 1 : nrow(inputs[['hrus']])) {           # Loop on each basin

    for (j in 1 : length(inputs[['pars']])) {       # Loop on each HRU in each basin

      # Pull out BASIN/HRU specific data for simhyd processing
      # 1) HRU - string
      hru = names(inputs[['pars']][j])
      
      # 3) HRU Area - numeric
      area = inputs[['hrus']][i, j + 1] # add one for an additional column
      
      # 4) Parameters - list
      pars = inputs[['pars']][[hru]]
      
      # 5) Met data (SUBSET FOR EXPEDIENCY AT THE MOMENT; SUBSET FOR TWO YEARS)
      met = data.frame(cbind('Date' = inputs[["met"]][["prc"]]$Date,
                             'prc' = inputs[["met"]][["prc"]][, i + 1],
                             'pet' = inputs[["met"]][["pet"]][, i + 1]))
      
      met$Date = as.POSIXct(met$Date, format = "%m/%d/%Y")
      
      met[, 2 : 3] = as.numeric(unlist(met[, 2 : 3]))
      
      met = met[which(met$Date >= strDate & met$Date <= endDate), ]
      
      # 5) Pass the above parameters to the simhyd processing function to calculate lateral flows
      #    Returns a data frame to the list element
      qData[[i]][[j]] = proc_simhyd(area, pars, met)

    }
  }
  
  end = as.numeric(Sys.time())
  
  print(paste0("Processing time: ", round((end - str) / 60, 1), " minutes"))
  
  return(qData)
  
}
