calib_preproc <- function(qData, qGage, calBas, strCal, endCal) {
  
  library(lubridate)
  
  # qGage = inputs[['flow']] # DELETE WHEN DONE!!!!

  qGage$Date = as.POSIXct(qGage$Date, origin = "1970-01-01", tz = "America/Los_Angeles")
  
  qClDat = qData[[calBas]][['TOT']]

  qClDat = qClDat[, -c(2 : 4)]

  qClDat$Date = as.POSIXct(qClDat$Date, origin = "1970-01-01", tz = "America/Los_Angeles")

  colnames(qClDat)[colnames(qClDat)=="ORTE"] <- "MDL_Q"

  # ________________________________________________________________
  # NEED TO REGULARIZE THE GAGE DATA TO THE FORMAT OF THE MODEL DATA
  # ________________________________________________________________

  qClDat = merge(qClDat, qGage, by.x = 'Date', by.y = 'Date', all.x = TRUE)
  
  colnames(qClDat)[colnames(qClDat)=="QCFS"] <- "GGE_Q"
  
  qClDat = qClDat[which(qClDat$Date >= strCal & qClDat$Date <= endCal), ]
  
  return(qClDat)

}
