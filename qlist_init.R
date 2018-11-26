# Initialize the list that will house all of the flow data

qlist_init <- function(bas, hru) { # Basin names then HRU names
   
  bas = inputs[['hrus']]$BASIN
  
  hru = names(inputs[['pars']])
  
  qPrt = list('flws' = NULL) 
     
  qHRU = qData = list()
     
  for (m in bas) {
  
    for (n in hru) {
    
      qHRU[[n]] = qPrt  
    
    }
    
    qData[[m]] = qHRU
  }
  
  return(qData)
  
}
