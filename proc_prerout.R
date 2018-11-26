proc_prerout = function(qData) {
  
  for (i in 1 : length(qData)) {           # Loop on each basin

    qData[[i]][['TOT']] = qData[[i]][["FRL"]]     # Initialize quickflow df for routing and set to 0
    
    qData[[i]][['TOT']]$OQCK = qData[[i]][['TOT']]$OBAS = 0

    for (j in 1 : (length(qData[[i]]) - 1)) {       # Loop on each HRU in each basin

      # Add the quickflows from each HRU in the basin together
      qData[[i]][['TOT']]$OQCK = qData[[i]][['TOT']]$OQCK + qData[[i]][[j]]$OQCK
       
      # Add the baseflows from each HRU in the basin together
      qData[[i]][['TOT']]$OBAS = qData[[i]][['TOT']]$OBAS + qData[[i]][[j]]$OBAS
      
    }
  }
  
  return(qData)

}
