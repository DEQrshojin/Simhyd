#' Read stream flow data from a csv
#'
#' @param flwFile The filepath to the flow data csv.
#' @return A data frames of \code{flwSlz} flow data.
#' @examples
#' read_flow('file/path/to/flow_data.csv')

read_flow = function(flwFile, strDate, endDate) {

  library(reshape2)
  library(lubridate)

  # Read flow data for calibration
  flw = read.csv(flwFile)

  # Aggregate into mean daily flows
  flw$DATE2 = as.Date(flw$DATE, format = "%m/%d/%Y %H:%M")
  
  flwSlz = flw[, c(2, 4)]
  
  flwSlz$Q = 1
  
  flwSlz = dcast(flwSlz, DATE2 ~ Q, mean, value.var = 'Siletz_5500I')
  
  names(flwSlz) = c("Date", "QCFS")
  
  flwSlz$Date = as.POSIXct(flwSlz$Date, format = "%Y-%m-%d", tz = "America/Los_Angeles") + hours(7)

  # Truncate to the modeling start and end dates
  flwSlz = flwSlz[which(flwSlz$Date >= strDate & flwSlz$Date <= endDate), ]

  return(flwSlz)

}


