
for (i in 1) {
  library(hydroGOF)
  library(ggplot2)
  library(scales)

  # Load the functions ----
  calPath <- paste0(rPath, 'calibration/')
  
  calFiles <- list.files(pattern = "[.]R$", path = calPath, full.names = TRUE)
  
  sapply(calFiles[-grep("calib_simhyd", calFiles)], source) # Source all of the functions except this one
  
  # CALIBRATION ----
  strCal <- '2008-10-01' # Start the calibration 1 year after the start of the model run (warm-up)
  endCal <- '2014-09-30'
  strCal <- as.POSIXct(strCal, format = '%Y-%m-%d')
  endCal <- as.POSIXct(endCal, format = '%Y-%m-%d')
  
  # This list provides the constituents to be included in the calibration. For the analysis period
  # The initial iteration of the program will only allow year-round, or one of the four seasons.
  
  objFun <- list('dayNSE' = TRUE,            # Daily Nash-Sutcliffe Efficiency
                 'monNSE' = TRUE,            # Monthly Nash-Sutcliffe Efficiency
                 'flwDur' = TRUE,            # Flow Duration Curves
                 'transf' = NULL,            # Specify a transformation of the data (none = NULL)
                 'annPer' = c('year round')) # Period of assessment; e.g.'winter', 'summer'
  
  # Calibration basin
  calBas <- 1 # Should build this into the shapefile
  
  fdcPar <- c(100, 1) # First number is the lower limit of flow percentage, second is the step
  
  qGage <- inputs[['flow']]
  
  # Pre-process the data for calibration
  calDat = calib_preproc(qData, qGage, calBas, strCal, endCal)
  
  calStt = list('dayNSE' = NULL,           # Daily Nash-Sutcliffe Efficiency
                'monNSE' = NULL,           # Monthly Nash-Sutcliffe Efficiency
                'fdcRMSE' = NULL)          # Flow duration curve RMSE
  
  trnsFun = objFun[['transf']]
  
  if(!is.null(objFun[['dayNSE']])) {
    calStt[['dayNSE']] = NSE(calDat$MDL_Q, calDat$GGE_Q, na.rm = TRUE, FUN = trnsFun)
  }
  
  if (!is.null(objFun[['monNSE']])) {
    calStt[['monNSE']] = calib_monNSE(calDat, trnsFun)
  }
  
  if (!is.null(objFun[['flwDur']])) {
    calStt[['fdcRMSE']] = calib_FDC(calDat, fdcPar)
  }
  
  # Print out calibration graphs 1) timeseries 2) scatterplot 3) ?????
  calPlot = ggplot(data = calDat) +
    geom_line(aes(x = Date, y = MDL_Q), size = 1.1, color = "blue") + 
    geom_point(aes(x = Date, y = GGE_Q), size = 1.2, color = 'red') + 
    scale_y_log10(limits = c(10, 50000)) + 
    scale_x_datetime(limits = c(strCal, endCal),
                     breaks = date_breaks("3 months"),
                     labels = date_format("%m/%d")) +
    xlab("Date") + ylab("Flow (cfs)") +
    theme_bw() + theme(legend.position = c(0, 1),
                       panel.grid.minor=element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 13),
                       axis.title.y = element_text(size = 13),
                       axis.text.y = element_text(size = 13),
                       plot.title = element_text(size = 13, hjust = 0.5))
  
  ggsave(filename = 'calibration_plot.png', plot = calPlot, path = datPath, width = 15,
         height = 10, dpi = 300, units = 'in')
  
  sctPlot = ggplot(data = calDat) + 
    geom_point(aes(x = GGE_Q, y = MDL_Q), size = 1.2) + 
    xlab("Gaged flows (cfs)") + ylab("Modeled flow (cfs)") +
    theme_bw() + theme(legend.position = c(0, 1),
                       panel.grid.minor=element_blank(),
                       axis.text.x = element_text(size = 13),
                       axis.title.y = element_text(size = 13),
                       axis.text.y = element_text(size = 13),
                       plot.title = element_text(size = 13, hjust = 0.5)) + 
    scale_x_log10(limits = c(10, 40000)) + scale_y_log10(limits = c(10, 40000))
  
  ggsave(filename = 'scatter_plot.png', plot = sctPlot, path = datPath, width = 15,
         height = 10, dpi = 300, units = 'in')
}
