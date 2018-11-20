# SIMHYD MODEL
# Reading inputs from external sources
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ

# Eventually add in capability to add landuse raster and catchment shapefile

read_simhyd_inputs = function(parFile = NULL,
                              prcFile = NULL,
                              petFile = NULL,
                              flwFile = NULL) {
    # LIBRARIES ----
    library(reshape2)
    
    # Ideally this will allow flexible file input, flexible raster/shapefile, etc.
    
    options(stringsAsFactors = FALSE)
    
    # 1) Read in parameters
    par = read.csv(parFile)
    
    pars = list()
    
    # Parse out each parameter into a list
    for (i in 1 : nrow(par)) {
        
        pars[[i]] = par[i, 4]
        
        names(pars)[i] = par[i, 3]
        
    }
    
    # 4) Read input meteorological data - must be in .csv
    # Read rainfall
    prc = read.csv(prcFile)
    
    # Read PET
    pet = read.csv(petFile)
    
    # aggregate into one timeseries for now
    dataDF = data.frame(cbind(prc$DATE,
                              apply(prc[, 2 : 18], 1, mean),
                              apply(pet[, 2 : 18], 1, mean)))
    
    dataDF$X1 = as.Date(dataDF$X1, "%m/%d/%Y")
    dataDF$X2 = as.numeric(dataDF$X2)
    dataDF$X3 = as.numeric(dataDF$X3)
    
    # 5) Read flow data for calibration
    flw = read.csv(flwFile)
    
    # Merge continuous readings into daily means for Siletz
    flw$DATE2 = as.Date(flw$DATE, "%m/%d/%Y %H:%M", origin = "1970-01-01")
    flwSlz = flw[, c(2, 4)]
    flwSlz$Q = 1
    flwSlz = dcast(flwSlz, DATE2 ~ Q, mean, value.var = 'Siletz_5500I')
    
    # Merge into main data fram (dataDF)
    dataDF = merge(dataDF, flwSlz, by.x = 'X1', by.y = 'DATE2', all.x = TRUE)
    names(dataDF) = c("Date", "P", "PET", "Q")
    
    inputs = list('parameters' = pars, 'inputs' = dataDF)
    
    return(inputs)
    
    # 2) Read landuse; assume a raster of discrete HRUs - IN DEVELOPMENT
    # 3) Read catchment shapefile - IN DEVELOPMENT
    
}

