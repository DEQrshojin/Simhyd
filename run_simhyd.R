# RUN SIMHYD
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# SimHyd Watershed Model with Routing

# To dos:
# 1) Fix the calibration scripts
# 2) Write in the reading from a control file
# 3) Routing ON/OFF switch


for (i in 1) {

  # INIT PROC TIME ----
  str = as.numeric(Sys.time())
  
  # Time inputs
  dt <- 1
  
  strDate <- '2011-10-01'
  endDate <- '2011-10-31'
  strDate <- as.POSIXct(strDate, format = '%Y-%m-%d')
  endDate <- as.POSIXct(endDate, format = '%Y-%m-%d')
  
  # Directories
  rPath <- 'E:/R/simhyd/simhyd/'
  cPath <- paste0(rPath, 'calibration/')
  datPath <- paste0(rPath, 'data/')
  
  # LOAD FUNCTIONS ----
  # Load requisite function - DEPRECATE WHEN THIS IS A PACKAGE!
  rFiles <- list.files(pattern = "[.]R$", path = rPath, full.names = TRUE) # Generate list of R fils
  cFiles <- list.files(pattern = "[.]R$", path = cPath, full.names = TRUE) # Generate list of R fils
  sapply(rFiles[-grep("run_simhyd", rFiles)], source) # Source all of the functions except this one
  sapply(cFiles, source) # Source all of the functions except this one
  
  # READ DATA ----
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
  
  # SIMHYD AND ROUTING PROCESSING ----
  qData <- proc_model(inputs)

  end <- as.numeric(Sys.time())
  
  print(paste0("Processing time: ", round((end - str) / 60, 2), " minutes"))
  
}

# CALIBRATION ----
# strCal <- '2008-10-01' # Seven-year period around the NLCD 2011 data
# endCal <- '2014-09-30'
# calBas <- 1 # Should build this into the shapefile
# calIn <- list('qmd' = qData,
#               'qgg' = inputs[['flow']],
#               'dir' = datPath,
#               'bas' = calBas,
#               'str' = strCal,
#               'end' = endCal)
# 
# calStt <- calib_simhyd(calIn)

# wqData <- wq_emcdwc(qData, datPath)

# INPUT BLOCK -- IN PLACE OF CONTROL FILE FOR NOW ----
#____________________________________________________#

# READ CONTROL FILE
# cf_data = read_control(controlFile)  
