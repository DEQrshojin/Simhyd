# Time inputs
dt <- 1 # Time step

strDate <- '1991-10-01'
endDate <- '1992-10-01'

strDate <- as.POSIXct(strDate, format = '%Y-%m-%d')
endDate <- as.POSIXct(endDate, format = '%Y-%m-%d')

# Directories
rPath <- 'C:/Users/ryans/Desktop/001_RMS/002_projects/scripts/SimHyd/'
datPath <- paste0(rPath, 'data/')

# Load requisite function - DEPRECATE WHEN THIS IS A PACKAGE!
rFiles <- list.files(pattern = "[.]R$", path = rPath, full.names = TRUE) # Generate list of R files

sapply(rFiles[-grep("run_simhyd", rFiles)], source) # Source all of the functions except this one

# Vector of full path names to input files
inFiles <- c(paste0(datPath, 'par.csv'),                    # for parameters
            paste0(datPath, 'p.csv'),                      # for precipitation data
            paste0(datPath, 'pet.csv'),                    # for PET data
            paste0(datPath, 'q.csv'),                      # for calibration flow data
            paste0(datPath, 'hru.csv'),                    # for hydro response unit areas
            paste0(datPath, 'siletz_catchments_HSPF.shp'), # for downstream flow links
            paste0(datPath, 'rte.csv'))                    # for routing parameters     

# Go git data!
inputs <- read_inputs(parFile = inFiles[1],
                     prcFile = inFiles[2],
                     petFile = inFiles[3],
                     flwFile = inFiles[4],
                     hruFile = inFiles[5],
                     shpFile = inFiles[6],
                     rteFile = inFiles[7])

# Initialize the list that will house all of the lateral catchment in-flows
qData <- qlist_init(inputs[['hrus']]$BASIN, names(inputs[['pars']])) # Basin names then HRU names

# Send inputs, routC and qData to proc_presim which will stage the info and send on to proc_simhyd
qData <- proc_presim(inputs, qData, strDate, endDate)

# Process stream routing - Muskingum routing used
# Need to pass flow (qData) routing parameters (PDSL[K], PIOB[x]), catch links, and time step dt, 
qData <- proc_routing(qData, inputs[['rte']], inputs[['lnks']], dt)






