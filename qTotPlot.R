qTotPlot = function(qData) {
  
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  qTotal = data.frame(matrix(0, nrow = nrow(qData[[1]][[1]]),
                        ncol = length(qData) + 1), stringsAsFactors = FALSE)
  
  names(qTotal) = c('Date', paste0('B', 1 : length(qData)))
  
  qTotal$Date = qData[[1]][[1]][['Date']]
  
  for (i in 1 : length(qData)) {
    
    qTotal[, i + 1] = qData[[i]][['TOT']][, 5]
    
  }
  
  qTotal = melt(qTotal, id.vars = 'Date')

  inputs[["flow"]]$Date = as.POSIXct(inputs[["flow"]]$Date, origin = "1970-01-01") + hours(7)
  
  qTPlot = ggplot() +
           geom_line(data = qTotal,
                     aes(x = Date, y = value, group = variable, color = variable),
                     size = 1.1) + 
           geom_line(data = inputs[["flow"]], aes( x = Date, y = QCFS)) + 
           scale_y_log10(limits = c(1, 10000)) + 
           scale_x_datetime(limits = c(strDate, endDate))

  ggsave(filename = 'qTPlot.jpg',
         plot = qTPlot,
         path = datPath,
         width = 7.5,
         height = 10,
         dpi = 300,
         units = 'in')
  
}
