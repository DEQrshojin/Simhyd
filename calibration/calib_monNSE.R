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
