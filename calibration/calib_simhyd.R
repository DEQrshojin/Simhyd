
calib_simhyd <- function(calIn) {

  library(hydroGOF)

  # LOAD FUNCTIONS DUNNO IF THIS IS NECESSARY ANYMORE $&@#&$@&%$#%^$#&^#%&*%$@^#@$&----
  calPath <- paste0(calIn[['dir']], 'calibration/')
  # calFiles <- list.files(pattern = "[.]R$", path = calPath, full.names = TRUE)
  # sapply(calFiles[-grep("calib_simhyd", calFiles)], source) # Source functions
 
  # CALIBRATION ----
  strCal <- as.POSIXct(calIn[['str']], format = '%Y-%m-%d')
  endCal <- as.POSIXct(calIn[['end']], format = '%Y-%m-%d')
  qData <- calIn[['qmd']]
  qGage <- calIn[['qgg']]
  calBas <- calIn[['bas']]

  # Pre-process the data for calibration
  calDat = calib_preproc(qData, qGage, calBas, strCal, endCal)
  calStt = list('dayNSE' = NULL,           # Daily Nash-Sutcliffe Efficiency
                'monNSE' = NULL,           # Monthly Nash-Sutcliffe Efficiency
                'fdcRMSE' = NULL)          # Flow duration curve RMSE
  calStt[['dayNSE']] = NSE(calDat$MDL_Q, calDat$GGE_Q, na.rm = TRUE, FUN = NULL)
  calStt[['monNSE']] = calib_monNSE(calDat)
  calStt[['fdcRMSE']] = calib_FDC(calDat, calIn[['dir']])
  plot_calib(calDat, strCal, endCal)
  
  return(calStt)

}  
  
calib_preproc <- function(qData, qGage, calBas, strCal, endCal) {

  library(lubridate)
  qGage$Date = as.POSIXct(qGage$Date, origin = "1970-01-01", tz = "America/Los_Angeles")
  qMod = qData[[calBas]][['TOT']] # Read in the flows from the calibration basin  
  qMod = qMod[, -c(2 : 4)]
  qMod$Date = as.POSIXct(qMod$Date, origin = "1970-01-01", tz = "America/Los_Angeles")
  colnames(qMod)[colnames(qMod) == "ORTE"] <- "MDL_Q"
  qClDat = merge(qMod, qGage, by.x = 'Date', by.y = 'Date', all.x = TRUE)
  colnames(qClDat)[colnames(qClDat) == "QCFS"] <- "GGE_Q"
  qClDat = qClDat[which(qClDat$Date >= strCal & qClDat$Date <= endCal), ]
  
  return(qClDat)
  
}

calib_monNSE = function(calData) {
  
  library(hydroGOF)
  library(reshape2)

  # Calculate daily volumes in ac-ft
  calDat$MDL_V = calDat$MDL_Q * 86400 / 43560
  calDat$GGE_V = calDat$GGE_Q * 86400 / 43560
  # Set month/year for aggregation
  calDat$YR = year(calDat$Date)
  calDat$Mon = month(calDat$Date)
  # reshape the table into volumes per month
  monVol = calDat[, -c(2, 3)]
  monVol = melt(monVol, id.vars = c('Date', 'YR', 'Mon'))
  monVol = dcast(monVol, YR + Mon ~ variable, value.var = 'value', fun.aggregate = sum)
  monNSE = NSE(monVol$MDL_V, monVol$GGE_V, na.rm = TRUE, FUN = NULL)
  
  return(monNSE)
  
}

calib_FDC = function(calData, datPath) {
  
  library(ggplot2)
  
  # Initialize percentages vector
  flwDur = data.frame('PCT' = seq(from = 1, to = 100, by = 1) / 100)
  # Calculate percentiles
  flwDur$MDL_Q = quantile(calDat$MDL_Q, flwDur$PCT)
  flwDur$GGE_Q = quantile(calDat$GGE_Q, flwDur$PCT)
  # FOR NOW USE RMSE FOR COMPARISON OF FDC
  fdcRMSE = rmse(flwDur$GGE_Q, flwDur$MDL_Q)
  # Reshape for graphing
  flwDurP = melt(flwDur, id.vars = 'PCT')
  fdcPlot = ggplot(data = flwDurP) +
    geom_line(aes(x = PCT, y = value, group = variable, color = variable), size = 1.1) + 
    scale_y_log10(limits = c(50, 50000)) + 
    scale_x_reverse(limits = c(1, 0), breaks = c(seq(1, 0, by = -0.1))) +
    xlab("PCT") + ylab("Flow (cfs)") +
    theme_bw() + theme(legend.position = c(0.2, 0.8),
                       panel.grid.minor=element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 16),
                       axis.title.y = element_text(size = 16),
                       axis.text.y = element_text(size = 16),
                       plot.title = element_text(size = 16, hjust = 0.5))
  ggsave(filename = 'fdc_plot.jpg', plot = fdcPlot, path = datPath, width = 15,
         height = 10, dpi = 300, units = 'in')
  
  return(fdcRMSE)
  
}

plot_calib <- function(calDat, strCal, endCal) {

  library(ggplot2)
  library(scales)

  # Print out calibration graphs 1) timeseries 2) scatterplot 3) ?????
  calPlot = ggplot(data = calDat) +
            geom_line(aes(x = Date, y = MDL_Q), size = 0.8, color = "blue") + 
            geom_point(aes(x = Date, y = GGE_Q), size = 2, shape = 2) +
            scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 2000)) +
            # scale_y_log10(limits = c(10, 10000), breaks = c(10, 100, 1000, 10000)) +
            scale_x_datetime(limits = c(strCal, endCal),
                             breaks = date_breaks("3 months"),
                             labels = date_format("%m-%Y")) +
            xlab("Date") + ylab("Flow (cfs)") +
            theme_bw() + theme(legend.position = c(0, 1),
                               panel.grid.minor=element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
                               axis.title.y = element_text(size = 16),
                               axis.text.y = element_text(size = 16),
                               plot.title = element_text(size = 16, hjust = 0.5))
  
  ggsave(filename = 'calibration_plot_non_log10.jpg', plot = calPlot, path = datPath, width = 15,
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
  
  ggsave(filename = 'scatter_plot.jpg', plot = sctPlot, path = datPath, width = 15,
         height = 10, dpi = 300, units = 'in')  

}

