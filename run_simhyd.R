# RUN SIMHYD
# Ryan Shojinaga, Oregon DEQ, shojinaga.ryan@deq.state.or.us
# SimHyd Watershed Model with Routing

for (i in 1) {

  # INIT PROC TIME ----
  str = as.numeric(Sys.time())
  
  # INPUT BLOCK -- IN PLACE OF CONTROL FILE FOR NOW ----
  #____________________________________________________#
  
  # READ CONTROL FILE
  # cf_data = read_control(controlFile)  
  
  # Time inputs
  dt <- 1
  
  strDate <- '2007-09-01' # Seveb-year period around the NLCD 2011 data
  endDate <- '2014-09-30'
  strDate <- as.POSIXct(strDate, format = '%Y-%m-%d')
  endDate <- as.POSIXct(endDate, format = '%Y-%m-%d')
  
  # Directories
  rPath <- 'C:/Users/ryans/Desktop/001_RMS/002_projects/scripts/simhyd_v2/'
  datPath <- paste0(rPath, 'data_test_one/')
  
  # LOAD FUNCTIONS ----
  # Load requisite function - DEPRECATE WHEN THIS IS A PACKAGE!
  rFiles <- list.files(pattern = "[.]R$", path = rPath, full.names = TRUE) # Generate list of R fils
  
  sapply(rFiles[-grep("run_simhyd", rFiles)], source) # Source all of the functions except this one
  
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

  # CALIBRATION ----
  strDate <- '2008-10-01' # Seven-year period around the NLCD 2011 data
  endDate <- '2014-09-30'
  
  calBas <- 1 # Should build this into the shapefile
  fdcPar <- c(100, 1) # First number is the lower limit of flow percentage, second is the step

  calIn <- list('flw' = qData,
                'dir' = rPath,
                'bas' = calBas,
                'fdc' = fdcPar,
                'str' = strCal,
                'end' = endCal)
  
  calStt <- calib_symhyd(calIn)
  
  end <- as.numeric(Sys.time())
  
  print(paste0("Processing time: ", round((end - str) / 60, 2), " minutes"))
  
}


