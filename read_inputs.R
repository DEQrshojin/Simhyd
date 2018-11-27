read_inputs = function(parFile = NULL,
                       prcFile = NULL,
                       petFile = NULL,
                       flwFile = NULL,
                       hruFile = NULL,
                       shpFile = NULL,
                       rteFile = NULL) {

  # LIBRARIES ----
  library(lubridate)

  options(stringsAsFactors = FALSE)

  # 1) Read in parameters
  pars = read_pars(parFile)

  # 2) Read input meteorological data - must be in .csv
  met = read_met(prcFile, petFile)

  # 3) Read flow data for calibration
  flow = read_flow(flwFile)

  # 4) Read landuse; from csv for now
  hrus = read_hru(hruFile)

  # 5) Downstream flow links & Convert the downstream flow links into a process order
  lnks = read_lnks(shpFile)
  
  lnks = proc_lnks(lnks)

  # 6) Read basin stream routing parameters
  rte = read_rout(rteFile)
  
  # 6) Interpret routing from basin slope and reach length (smallest is identity = 1)
  # rout = read_rout(shpFile)
  # relate catchment size to flow and velocity, therefore relative flow delay??

  # 7) Aggregate the inputs
  inputs = list('pars' = pars,
                'met' = met,
                'flow' = flow,
                'hrus' = hrus,
                'lnks' = lnks,
                'rte' = rte)

  return(inputs)

}

