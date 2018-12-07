proc_wq <- function(qData, datPath) {
  
  # Read in the Dry-weather and Event Mean concentrations
  wqFil = paste0(datPath, "wq_emcdwc.csv")
  wqPar = read.csv(wqFil)

  # Isolate the water quality constituents
  wqNme = names(wqPar)
  wqNme = wqNme[2 : length(wqNme)]
  wqCns = gsub("_.*", "", wqNme)
  wqCns = unique(wqCns)

  # Initialize list of n elements equivalent to qData, where n is the number of WQ constituents
  wqData = list()
  for (cons in wqCns) {
    wqData[[cons]] = qData
  }
  
  wqDataBU = wqData
  
  # Process all lateral flow loads first
  flow2LoadConversion = 28.316846592 * 86400 / 10^6   # From cfs*mg/L to kg/day
  
  for (i in 1 : length(wqData)) {                     # Loop on each parameter
    for (j in 1 : length(wqData[[1]])) {              # Loop on each basin
      for (k in 1 : nrow(wqPar)) {                    # Loop on each HRU in each basin
        # Pull out the EMC
        EMC = wqPar[k, 2 * i + 1]
        # Pull out the DWC
        DWC = wqPar[k, 2 * i]
        # Event Mean Concentration (EMC) converted to load
        wqData[[i]][[j]][[k]][[2]] = wqData[[i]][[j]][[k]][[2]] * EMC * flow2LoadConversion
        # Dry Weather Concentration (DWC) converted to load
        wqData[[i]][[j]][[k]][[3]] = wqData[[i]][[j]][[k]][[3]] * DWC * flow2LoadConversion
        # Combine all of the loads for each basin for each day
        if (k == 1) {
          wqData[[i]][[j]][['TOT']][[2]] = wqData[[i]][[j]][[k]][[2]]       # EMC
          wqData[[i]][[j]][['TOT']][[3]] = wqData[[i]][[j]][[k]][[3]]       # DWC
        } else {
          wqData[[i]][[j]][['TOT']][[2]] = wqData[[i]][[j]][['TOT']][[2]] + # EMC
                                           wqData[[i]][[j]][[k]][[2]]
          wqData[[i]][[j]][['TOT']][[3]] = wqData[[i]][[j]][['TOT']][[3]] + # DWC
                                           wqData[[i]][[j]][[k]][[3]]
        }
          
        
        
      }
    }
  }
  
  
  
}

