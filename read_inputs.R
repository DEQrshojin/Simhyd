read_inputs = function(parFile = NULL, prcFile = NULL, petFile = NULL, flwFile = NULL,
                       hruFile = NULL, shpFile = NULL, rteFile = NULL,
                       strDate = NULL, endDate = NULL) {
  options(stringsAsFactors = FALSE)

  # LIBRARIES ----
  library(lubridate)

  # 1) Read in parameters
  pars = read_pars(parFile)
  # 2) Read input meteorological data - must be in .csv
  met = read_met(prcFile, petFile, strDate, endDate)
  # 3) Read flow data for calibration
  flow = read_flow(flwFile, strDate, endDate)
  # 4) Read landuse; from csv for now
  hrus = read_hru(hruFile)
  # 5) Downstream flow links & converts the downstream flow links into processing order
  lnks = read_lnks(shpFile)
  lnks = proc_lnks(lnks)
  # 6) Read basin stream routing parameters
  rte = read_rout(rteFile)
  # 7) Aggregate the inputs
  inputs = list('pars' = pars,
                'met' = met,
                'flow' = flow,
                'hrus' = hrus,
                'lnks' = lnks,
                'rte' = rte,
                'date' = c(strDate, endDate))
  return(inputs)
}


#' Read hydrologic parameters from a csv
#'
#' @param parFile The filepath to the HRU parameters csv.
#' @return A list of HRU specific \code{pars} parameter characteristics.
#' @examples
#' read_pars('file/path/to/parameters.csv')

read_pars <- function(parFile) {
  
  options(stringsAsFactors = FALSE)
  par = read.csv(parFile)
  hruNames = colnames(par)
  hruNames = hruNames[5 : length(hruNames)]
  hruPar = list()
  pars = list()
  
  # Parse out each parameter into a list
  for (i in 5 : length(par)) {    # iterate through each hru
    for (j in 1 : nrow(par)) {    # iterate through each parameter
      hruPar[[j]] = par[j, i]     # by each parameter
      names(hruPar)[j] = par[j, 4]#
    }
    pars[[i - 4]] = hruPar
    names(pars)[i - 4] = hruNames[i - 4]
  }
  return(pars)
}

#' Read rainfall and PET data from a csv
#'
#' @param prcFile The filepath to the precipitation data.
#' @param petFile The filepath to the PET data.
#' @return A list of data frames of \code{prcFile} and \code{petFile}.
#' @examples
#' read_met('file/path/to/precip_data.csv', 'file/path/to/pet_data.csv')

read_met = function(prcFile, petFile, strDate, endDate) { 
  
  # Read Precip
  prc = read.csv(prcFile)
  # Read PET
  pet = read.csv(petFile)
  # Rename columns to Basin IDs
  basID = paste0("B", 1 : (length(prc) - 1))
  names(prc) = c("Date", basID)
  names(pet) = c("Date", basID)
  # Clip the data to the model dates
  prc$Date = as.POSIXct(prc$Date, format = "%m/%d/%Y")
  prc = prc[which(prc$Date >= strDate & prc$Date <= endDate), ]
  pet$Date = as.POSIXct(pet$Date, format = "%m/%d/%Y")
  pet = pet[which(pet$Date >= strDate & pet$Date <= endDate), ]
  # Aggregate into a list
  metData = list('prc' = prc, 'pet' = pet)
  
  return(metData)
  
}

#' Read stream flow data from a csv
#'
#' @param flwFile The filepath to the flow data csv.
#' @return A data frames of \code{flwSlz} flow data.
#' @examples
#' read_flow('file/path/to/flow_data.csv')

read_flow = function(flwFile, strDate, endDate) {
  
  library(reshape2)
  library(lubridate)
  
  # Read flow data for calibration
  flw = read.csv(flwFile)
  # Aggregate into mean daily flows
  flw$DATE2 = as.Date(flw$DATE, format = "%m/%d/%Y %H:%M")
  flwSlz = flw[, c(2, 4)]
  flwSlz$Q = 1
  flwSlz = dcast(flwSlz, DATE2 ~ Q, mean, value.var = 'Siletz_5500I')
  names(flwSlz) = c("Date", "QCFS")
  flwSlz$Date = as.POSIXct(flwSlz$Date, format = "%Y-%m-%d", tz = "America/Los_Angeles")
  flwSlz$Date = ifelse(hour(flwSlz$Date) == 17, flwSlz$Date + hours(7), flwSlz$Date + hours(8))
  # Truncate to the modeling start and end dates
  flwSlz = flwSlz[which(flwSlz$Date >= strDate & flwSlz$Date <= endDate), ]
  
  return(flwSlz)
}

#' Read catchment by catchment hydro response units
#'
#' @param hruFile The filepath to the precipitation data.
#' @return A list of lists of \code{hrus} areas per catchment.
#' @examples
#' read_met('file/path/to/precip_data.csv', 'file/path/to/pet_data.csv')

read_hru = function(hruFile) {
  
  # Read PET
  hrus = read.csv(hruFile)

  return(hrus)
  
}

#' Read the catchment links from a shapefile
#'
#' @param shpFile The filepath to the precipitation data.
#' @return A list of data frames of \code{shpFile}
#' @examples
#' read_link('file/path/to/catchments.shp')

read_lnks = function(shpFile) {
  
  library(raster)
  
  shpFile = shapefile(shpFile)
  lnks = data.frame(cbind('Basn' = as.numeric(shpFile@data[["HSPF_Bas"]]),
                          'DSBs' = as.numeric(shpFile@data[["DS_Basin"]])),
                    stringsAsFactors = FALSE)
  
  return(lnks)
  
}


proc_lnks = function(lnks) {
  
  # Make a list of each basin -- each list will have a vector of upstream basin(s) -- lists
  # are not cumulative, just the immediately upstream basin(s) for upstream inflow inputs
  # Basins with no upstream basins (headwaters) have a value of 0
  usBas = rep(list(0), nrow(lnks))
  twBas = unique(lnks$DSBs) # All tailwater basins (anything downstream of another basin)
  twBas = twBas[which(twBas != 0)] # remove zeros (0 = watershed outlet)
  for (basin in twBas) {
    usBas[[basin]] = lnks[which(lnks$DSBs == basin), 1]
  }
  # Make a vector of the basin processing order
  hwBas = lnks$Basn # All headwater basins
  hwBas = hwBas[!(hwBas %in% twBas)]
  # First order basins (headwaters)
  nthOrdBas = list()
  nthOrdBas[[1]] = procOrd = hwBas # initialize processing order
  n = 2 # start the counter for indexing the loop; n = 1 addressed with headwaters
  # subsequent order basins
  repeat {
    nthOrdBas[[n]] = lnks[which(lnks$Basn %in% nthOrdBas[[n - 1]]), 2]
    nthOrdBas[[n]] = unique(nthOrdBas[[n]]) # remove duplicates
    nthOrdBas[[n]] = nthOrdBas[[n]][which(nthOrdBas[[n]] != 0)] # remove zeros
    if (length(nthOrdBas[[n]]) == 0) { # exit when the nthOrdBas has no elements
      break
    }
    procOrd = c(procOrd, nthOrdBas[[n]])
    n = n + 1      
  }
  
  # Once the order vector has been created, keep the LAST indexed instance of dup basins
  # The first column is the process order, the second is the basin to process in that order
  tmpOrd = data.frame(cbind('ORD' = as.vector(tapply(seq_along(procOrd), procOrd, max)),
                            'BAS' = 1 : nrow(lnks)), stringsAsFactors = FALSE)
  tmpOrd = tmpOrd[order(tmpOrd$ORD), ]
  tmpOrd$ORD = 1 : nrow(lnks)
  rownames(tmpOrd) = 1 : nrow(lnks)
  procLnks = list('pOrd' = tmpOrd, 'cBas' = usBas)

  return(procLnks)
  
}

read_rout <- function(rteFile) {
  
  rte = read.csv(rteFile)
  
  return(rte)
  
}