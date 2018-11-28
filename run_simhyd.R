run_simhyd <- function() {

  str = as.numeric(Sys.time())
  
  # READ CONTROL FILE
  # cf_data = read_control(controlFile)  
  
  dt <- 1
  strDate <- '2009-10-01'
  endDate <- '2013-09-30'
  strDate <- as.POSIXct(strDate, format = '%Y-%m-%d')
  endDate <- as.POSIXct(endDate, format = '%Y-%m-%d')
  
  # Directories
  rPath <- 'E:/R/simhyd/simhyd/'
  datPath <- paste0(rPath, 'data/')
  
  # Load requisite function - DEPRECATE WHEN THIS IS A PACKAGE!
  rFiles <- list.files(pattern = "[.]R$", path = rPath, full.names = TRUE) # Generate list of R fils
  
  sapply(rFiles[-grep("run_simhyd", rFiles)], source) # Source all of the functions except this one

  # Vector of full path names to input files
  inFiles <- c(paste0(datPath, 'par.csv'),                    # for parameters
               paste0(datPath, 'p.csv'),                      # for precipitation data
               paste0(datPath, 'pet.csv'),                    # for PET data
               paste0(datPath, 'q.csv'),                      # for calibration flow data
               paste0(datPath, 'hru.csv'),                    # for hydro response unit areas
               paste0(datPath, 'siletz_basins.shp'),          # for downstream flow links
               paste0(datPath, 'rte.csv'))                    # for routing parameters     
  
  # Go git data!
  inputs <- read_inputs(parFile <- inFiles[1],
                        prcFile <- inFiles[2],
                        petFile <- inFiles[3],
                        flwFile <- inFiles[4],
                        hruFile <- inFiles[5],
                        shpFile <- inFiles[6],
                        rteFile <- inFiles[7],
                        strDate, endDate)
  
  # Initialize the list that will house all of the lateral catchment in-flows
  qData <- qlist_init(inputs[['hrus']]$BASIN, names(inputs[['pars']])) # Basin names then HRU names
  
  # Send inputs, routC and qData to proc_presim which will stage the info and send on to proc_simhyd
  qData <- proc_presim(inputs, qData, strDate, endDate)
  
  # Process stream routing - Muskingum routing used
  # Need to pass flow (qData) routing parameters (PDSL[K], PIOB[x]), catch links, and time step dt, 
  qData <- proc_routing(qData, inputs[['rte']], inputs[['lnks']], dt)
  
  # This list provides the constituents to be included in the calibration. For the analysis period
  # The initial iteration of the program will only allow year-round, or one of the four seasons.
  constituents <- list('dayNSE' = TRUE,
                       'monNSE' = TRUE,
                       'annVol' = TRUE,
                       'flwDur' = TRUE,
                       'annPer' = c('year round', 'winter', 'spring', 'summer', 'fall'))
  
  calib_simhyd(qData, inputs[['flow']], constituents)
  
  end <- as.numeric(Sys.time())
  
  print(paste0("Processing time: ", round((end - str) / 60, 2), " minutes"))
  
  return(qData)
  
}
