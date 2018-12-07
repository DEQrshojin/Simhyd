
proc_model <- function(inputs) {
  
  # Initialize the list that will house all of the lateral catchment in-flows
  qData <- qlist_init(inputs) # Basin names then HRU names
  # Send inputs, routC and qData to proc_presim which will stage the info and send on to proc_simhyd
  qData <- proc_presim(inputs, qData)
  # Process stream routing - Muskingum routing used
  qData <- proc_routing(inputs, qData)
  
  return(qData)
  
}

# Initialize the list that will house all of the flow data

qlist_init <- function(inputs) { # Basin names then HRU names
  
  bas = inputs[['hrus']]$BASIN
  hru = names(inputs[['pars']])
  qPrt = list('flws' = NULL) 
  qHRU = qData = list()
  for (m in bas) {
    for (n in hru) {
      qHRU[[n]] = qPrt  
    }
    qData[[m]] = qHRU
  }
  
  return(qData)
  
}

proc_presim <- function(inputs, qData) {
  
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
      # 5) Pass the above parameters to the simhyd processing function to calculate lateral flows
      #    Returns a data frame to the list element
      qData[[i]][[j]] = proc_simhyd(area, pars, met)
    }
  }
  
  return(qData)
  
}

proc_prerout = function(qData) {
  
  for (i in 1 : length(qData)) { # Loop on each basin
    # Initialize quickflow df for routing and set to 0
    qData[[i]][['TOT']] = qData[[i]][["FRL"]]
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

proc_routing <- function(inputs, qData) {
  
  # Pre-process flow data for routing aggregate quickflows for all hrus in a basin
  qData = proc_prerout(qData)
  # Calculate the Muskingum routing coefficients
  rte = inputs[['rte']] # DELETE WHEN DONE
  rte = routing_coefficients(rte)
  
  # Muskingum routing assumes a linear relationship between the Outflow at i, Inflow at i,
  # Inflow at i-1, and Outflow at i-1. Inflows include routed upstream inflows and lateral
  # inflows. Hence use the links relationships (upstream contributing basins [cBas]
  # and process order [pOrd]).
  pOrd = inputs[['lnks']][['pOrd']]$BAS
  cBas = inputs[['lnks']][['cBas']]
  # Hydrologic Flow Routing; C_0 x (RTRI)[i] + C_1 x RTRI[i-1] + C_2 x SMSK[i-1]
  for (bas in 1 : length(qData)) { # Iterate through each basin
    # n = the basin number being currently processed
    n = pOrd[bas]
    # m = contributing upstream basins for inflows
    m = unlist(cBas[n])
    # Initialize upstream inflow, routed outflow, and storage vectors for basin n
    qData[[n]][["TOT"]]$IUSO = 0 # Column 4
    qData[[n]][["TOT"]]$ORTE = 0 # Column 5
    qData[[n]][["TOT"]]$ST0R = 0 # Column 6
    # Aggregate upstream flows
    if (m[1] != 0) { # If m == 0 there are no upstream inflows (i.e., no upstream basins)
      for (x in m) {
        # Upstream inflow for basin n = itself + routed outflow + baseflow from US basin x in m
        qData[[n]][["TOT"]]$IUSO = qData[[n]][["TOT"]]$IUSO + qData[[x]][["TOT"]]$ORTE +
                                   qData[[x]][["TOT"]]$OBAS 
      }  
    }
    # Now take the inputs and route them
    for (day in 1 : nrow(qData[[n]][["TOT"]])) { # iterate on each day
      if(day == 1) {
        # Outlow[i] = Baseflow[i] + US Inflow[i] + Later Inflow[i]
        qData[[n]][['TOT']][day, 5] = qData[[n]][['TOT']][day, 3] + # Baseflow
                                      qData[[n]][['TOT']][day, 4] + # Upstream inflows
                                      qData[[n]][['TOT']][day, 2]   # Lateral inflows
        # Storage
        qData[[n]][['TOT']][day, 6] = 0 + (sum(qData[[n]][['TOT']][day, 2 : 4]) - 
                                      qData[[n]][['TOT']][day, 5]) * 86400 # (cf/day)
      } else {
        # Outlow[i] = Baseflow[i] + Inflow[i] + Inflow[i-1] + Outflow[i-1]
        # Turn this off to remove routing
        qData[[n]][['TOT']][day, 5] = qData[[n]][['TOT']][day, 3] +      # Baseflow [i]
                         rte[n, 5] * (qData[[n]][['TOT']][day, 2] +      # Lat inflows [i]
                                      qData[[n]][['TOT']][day, 4]) +     # U/S inflows [i]
                         rte[n, 6] * (qData[[n]][['TOT']][day - 1, 2] +  # Lat inflows [i-1]
                                      qData[[n]][['TOT']][day - 1, 4]) + # U/S inflows [i-1]
                         rte[n, 7] *  qData[[n]][['TOT']][day - 1, 5]    # Routed Outflow [i-1]
        # Storage
        qData[[n]][['TOT']][day, 6] = qData[[n]][['TOT']][day - 1, 6] +       # Storage [i-1]
                                      (sum(qData[[n]][['TOT']][day, 2 : 4]) - # Inflows [i]
                                      qData[[n]][['TOT']][day, 5]) * 86400    # Outflows [i]; cf/day
      }
    }
  }
  
  return(qData)
  
}

routing_coefficients = function(rte) {

  # D = k(1 - x) + 0.5 * dt -- dt = 1 until I program otherwise
  rte$D = rte$PDSL * (1 - rte$PIOB) + 0.5                   # 4
  rte$C_0 = (-rte$PDSL * rte$PIOB + 0.5) / rte$D            # 5 
  rte$C_1 = (rte$PDSL * rte$PIOB + 0.5) / rte$D             # 6
  rte$C_2 = (rte$PDSL * (1 - rte$PIOB) - 0.5) / rte$D       # 7
  rte$Chk1 = rte$C_0 + rte$C_1 + rte$C_2                    # 8
  rte$Chk2 = 0.5 / rte$PDSL                                 # 9
  rte$Chk2 = ifelse((rte$PIOB < rte$Chk2 & rte$Chk2 < 1 - rte$PIOB), 1, -1)
  
  return(rte)
  
}


