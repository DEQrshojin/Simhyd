proc_inflow_rout = function(qData) {
  
  for (day in 1 : nrow(qData[[n]][["TOT"]])) { # iterate on each day
    
    if(day == 1) {
      
      qData[[n]][['TOT']][day, 4] = qData[[n]][['TOT']][day, 3] + # Baseflow
                                    qData[[n]][['TOT']][day, 5] + # Upstream inflows
                                    qData[[n]][['TOT']][day, 2]   # Lateral inflows
      
    } else {  
      
      # Outlow[i] = Baseflow[i] + Inflow[i] + Inflow[i-1] + Outflow[i-1]
      qData[[n]][['TOT']][day, 4] = qData[[n]][['TOT']][day, 3] +                   # Baseflow [i] 
                                    rte[n, 5] * (qData[[n]][['TOT']][day, 2] +      # Lat Q [i]
                                                 qData[[n]][['TOT']][day - 1, 5]) + 
                                    rte[n, 6] * (qData[[n]][['TOT']][day - 1, 2] +  # Lat Q [i-1]
                                                 qData[[n]][['TOT']][day - 1, 5]) + 
                                    rte[n, 7] * qData[[n]][['TOT']][day - 1, 4]     # Out Q [i-1]
      
    }
  }
  
  return(qData)
  
}

