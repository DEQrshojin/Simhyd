read_inputs = function(parFile = NULL,
                       prcFile = NULL,
                       petFile = NULL,
                       flwFile = NULL,
                       hruFile = NULL,
                       shpFile = NULL) {

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

  # 5) Downstream flow links
  lnks = read_lnks(shpFile)

  inputs = list('pars' = pars,
                'met' = met,
                'flow' = flow,
                'hrus' = hrus,
                'lnks' = lnks)

  return(inputs)

}

