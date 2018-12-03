
calib_simhyd <- function(calIn) {

  library(hydroGOF)
  library(ggplot2)
  library(scales)

  # LOAD FUNCTIONS ----
  calPath <- paste0(calIn[['dir']], 'calibration/')
  calFiles <- list.files(pattern = "[.]R$", path = calPath, full.names = TRUE)
  sapply(calFiles[-grep("calib_simhyd", calFiles)], source) # Source functions
 
  # CALIBRATION ----
  strCal <- as.POSIXct(calIn[['str']], format = '%Y-%m-%d')
  endCal <- as.POSIXct(calIn[['end']], format = '%Y-%m-%d')
  qGage <- calIn[['flw']]
  calBas <- calIn[['bas']]
  qData <- calIn[['flw']]

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
    calStt[['monNSE']] = calib_monNSE(calDat)
  }
  if (!is.null(objFun[['flwDur']])) {
    calStt[['fdcRMSE']] = calib_FDC(calDat, fdcPar)
  }

  
  return(calStt)

}  
  
calib_preproc <- function(qData, qGage, calBas, strCal, endCal) {

  library(lubridate)
  qGage$Date = as.POSIXct(qGage$Date, origin = "1970-01-01", tz = "America/Los_Angeles")
  qClDat = qData[[calBas]][['TOT']]
  qClDat = qClDat[, -c(2 : 4)]
  qClDat$Date = as.POSIXct(qClDat$Date, origin = "1970-01-01", tz = "America/Los_Angeles")
  colnames(qClDat)[colnames(qClDat) == "ORTE"] <- "MDL_Q"
  qClDat = merge(qClDat, qGage, by.x = 'Date', by.y = 'Date', all.x = TRUE)
  colnames(qClDat)[colnames(qClDat) == "QCFS"] <- "GGE_Q"
  qClDat = qClDat[which(qClDat$Date >= strCal & qClDat$Date <= endCal), ]
  
  return(qClDat)
  
}

calib_monNSE = function(calData, trnsFun) {
  
  library(hydroGOF)
  library(reshape2)
  library(ggplot2) # DELETE WHEN DONE
  
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
  monNSE = NSE(monVol$MDL_V, monVol$GGE_V, na.rm = TRUE, FUN = trnsFun)
  
  return(monNSE)
  
}

calib_FDC = function(calData, fdcLim) {
  
  library(ggplot2)
  
  # Initialize percentages vector
  flwDur = data.frame('PCT' = seq(from = fdcPar[2], to = fdcPar[1], by = fdcPar[2]) / 100)
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
    scale_x_continuous(limits = c(0, 1)) +
    xlab("PCT") + ylab("Flow (cfs)") +
    theme_bw() + theme(legend.position = c(0.2, 0.8),
                       panel.grid.minor=element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 13),
                       axis.title.y = element_text(size = 13),
                       axis.text.y = element_text(size = 13),
                       plot.title = element_text(size = 13, hjust = 0.5))
  ggsave(filename = 'fdc_plot.jpg', plot = fdcPlot, path = datPath, width = 7.5,
         height = 10, dpi = 300, units = 'in')
  
  return(fdcRMSE)
  
}

plot_calib <- function(calDat, strCal, endCal) {

  # Print out calibration graphs 1) timeseries 2) scatterplot 3) ?????
  calPlot = ggplot(data = calDat) +
    geom_line(aes(x = Date, y = MDL_Q), size = 0.8, color = "blue") + 
    geom_point(aes(x = Date, y = GGE_Q), size = 2, shape = 2) +
    # scale_y_continuous(limits = c(0, 10000)) +
    scale_y_log10(limits = c(10, 10000)) +
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
  
  ggsave(filename = 'calibration_plot.jpg', plot = calPlot, path = datPath, width = 15,
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