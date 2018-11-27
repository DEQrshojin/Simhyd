#' Read stream flow data from a csv
#'
#' @param flwFile The filepath to the flow data csv.
#' @return A data frames of \code{flwSlz} flow data.
#' @examples
#' read_flow('file/path/to/flow_data.csv')

read_flow = function(flwFile) {

  library(reshape2)
  library(lubridate)

  # Read flow data for calibration
  flw = read.csv(flwFile)

  # Merge continuous readings into daily means
  flw$DATE2 = as.Date(flw$DATE, "%m/%d/%Y %H:%M", origin = "1970-01-01")
  
  flwSlz = flw[, c(2, 4)]
  
  flwSlz$Q = 1
  
  flwSlz = dcast(flwSlz, DATE2 ~ Q, mean, value.var = 'Siletz_5500I')
  
  names(flwSlz) = c("Date", "QCFS")

  return(flwSlz)

}


