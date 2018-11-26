proc_routing <- function(qData, rte, lnks, dt) {
  
  # qDataBU = qData
  # qData = qDataBU
  
  # Pre-process flow data for routing aggregate quickflows for all hrus in a basin
  qData = proc_prerout(qData)

  # Calculate the Muskingum routing coefficients
  rte = inputs[['rte']] # DELETE WHEN DONE
  
  rte = routing_coefficients(rte, dt)
  
  # Muskingum routing assumes a linear relationship between the Outflow at i, Inflow at i,
  # Inflow at i-1, and Outflow at i-1. Inflows include routed upstream inflows and lateral
  # inflows. Hence use the links relationships (upstream contributing basins [cBas]
  # and process order [pOrd]).
  
  pOrd = inputs[['lnks']][['pOrd']]$BAS
  
  cBas = inputs[['lnks']][['cBas']]
  
  str = as.numeric(Sys.time())
  
  # Hydrologic Flow Routing; C_0 x (RTRI)[i] + C_1 x RTRI[i-1] + C_2 x SMSK[i-1]
  for (bas in 1 : length(qData)) { # Iterate through each basin

    # n = the basin number being currently processed
    n = pOrd[bas]
    
    # m = contributing upstream basins for inflows
    m = unlist(cBas[n])

    # Initialize upstream inflow and routed outflow vectors for basin n
    qData[[n]][["TOT"]]$IUSO = 0

    qData[[n]][["TOT"]]$ORTE = 0    

    # Aggregate upstream flows
    if (m[1] != 0) { # If m == 0 there are no upstream inflows
      
      for (x in m) {
        
        # Upstream inflow for basin n = itself + routed outflow + baseflow from US basin x in m
        qData[[n]][["TOT"]]$IUSO = qData[[n]][["TOT"]]$IUSO +
                                   qData[[x]][["TOT"]]$ORTE +
                                   qData[[x]][["TOT"]]$OBAS 
        
      }  
    }
    
    # Now take the inputs and route them
    for (day in 1 : nrow(qData[[n]][["TOT"]])) { # iterate on each day

      # if(day == 1) {

        # Outlow[i] = Baseflow[i] + US Inflow[i] + Later Inflow[i]
        qData[[n]][['TOT']][day, 5] = qData[[n]][['TOT']][day, 3] + # Baseflow
                                      qData[[n]][['TOT']][day, 4] + # Upstream inflows
                                      qData[[n]][['TOT']][day, 2]   # Lateral inflows

      # } else {

        # Outlow[i] = Baseflow[i] + Inflow[i] + Inflow[i-1] + Outflow[i-1]
        # Turn this off to remove routing
        # qData[[n]][['TOT']][day, 5] = qData[[n]][['TOT']][day, 3] +      # Baseflow [i]
        #                  rte[n, 5] * (qData[[n]][['TOT']][day, 2] +      # Lat Q [i]
        #                               qData[[n]][['TOT']][day, 4]) +     # US Q [i]
        #                  rte[n, 6] * (qData[[n]][['TOT']][day - 1, 2] +  # Lat Q [i-1]
        #                               qData[[n]][['TOT']][day - 1, 4]) + # US Q [i-1]
        #                  rte[n, 7] *  qData[[n]][['TOT']][day - 1, 5]    # Out Q [i-1]

      # }
    }
  }
  
  end = as.numeric(Sys.time())
  
  print(paste0("Processing time: ", round((end - str) / 60, 1), " minutes"))
  
  return(qData)
  
}
