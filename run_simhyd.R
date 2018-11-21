
# Directories
rPath = 'E:/R/simhyd/simhyd/'
datPath = paste0(rPath, 'data/')

# Load requisite function - DEPRECATE WHEN THIS IS A PACKAGE!
sapply(list.files(pattern = "[.]R$", path = rPath, full.names = TRUE), source)

# Vector of full path names to input files
inFiles = c(paste0(datPath, 'par.csv'),                    # for parameters
            paste0(datPath, 'p.csv'),                      # for precipitation data
            paste0(datPath, 'pet.csv'),                    # for PET data
            paste0(datPath, 'q.csv'),                      # for calibration flow data
            paste0(datPath, 'hru.csv'),                    # for hydro response unit areas
            paste0(datPath, 'siletz_catchments_HSPF.shp')) # for downstream flow links

# Go git data!
inputs = read_inputs(parFile = inFiles[1],
                     prcFile = inFiles[2],
                     petFile = inFiles[3],
                     flwFile = inFiles[4],
                     hruFile = inFiles[5],
                     shpFile = inFiles[6])

# Convert the downstream flow links into a process order
lnks = proc_lnks(inputs[['lnks']])

# for (i in 1 : nrow(inputs[['hru']])) {            # Loop on each basin
#
#   for (j in 1 : length(inputs[['pars']])) {       # Loop on each HRU in each basin

    i = 3 # Basin # 3
    j = 6 # Grass High

    # Pull out BASIN/HRU specific data for simhyd processing
    # 1) HRU
    hru = names(inputs[["pars"]][[j]])

    # 2) HRU Area


    # 3) Parameters


    # 4) Met data (SUBSET FOR EXPEDIENCY AT THE MOMENT; SUBSET FOR TWO YEARS)


    # 5) Routing
    routC = routing_coeff(parm[["PSDL"]], parm[["PIOB"]], parm[["dt"]])

    # 6) Pass the above parameters to the simhyd processing function


#   }
#
# }


# parm = inputs[[1]]
#
# data = inputs[[2]][2923 : 5115, ] # Look at 5 years of data starting at 1995 (w 1 yr warm-up)
#
# routC = routing_coeff(parm[["PSDL"]], parm[["PIOB"]], parm[["dt"]])
#
# outflow = simhyd_processing(parm, data, hru, area, routC)
#
# data$Qmod = outflow$QCFS
#
# xlims = c(as.POSIXct('1995-10-01', "%Y-%m-%d", tz = "America/Los_Angeles"),
#           as.POSIXct('2000-10-01', "%Y-%m-%d", tz = "America/Los_Angeles"))
#
# ylims = c(1, 10000)
#
# qPlot = ggplot(data = data) + geom_line(aes(x = Date, y = Q), size = 0.5) +
#   geom_point(aes(x = Date, y = Q), size = 1.4, color = 'forestgreen') +
#   geom_line(aes(x = Date, y = Qmod), color = 'blue') +
#   scale_y_log10(limits = ylims) + scale_x_datetime(limits = xlims)
#
# ggsave(filename = paste0(datPath, "test_", iteration, ".png"), qPlot,
#   width = 16, height = 8.5, units = "in", dpi = 300)
#
#
