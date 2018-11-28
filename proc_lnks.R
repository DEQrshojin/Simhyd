
proc_lnks = function(lnks) {

  # Make a list of each basin -- each list will have a vector of upstream basin(s) -- lists
  # are not cumulative, just the immediately upstream basin(s) for upstream inflow inputs
  # Basins with no upstream basins (headwaters) have a value of 0
  usBas = rep(list(0), nrow(lnks))
  
  twBas = unique(lnks$DSBs) # All tailwater basins (anything downstream of another basin)
  
  twBas = twBas[which(twBas != 0)] # remove zeros (0 = watershed outlet)

  for (basin in twBas) {
    
    usBas[[basin]] = lnks[which(lnks$DSBs == basin), 1]
    
  }

  # Make a vector of the basin processing order
  hwBas = lnks$Basn # All headwater basins
  
  hwBas = hwBas[!(hwBas %in% twBas)]
  
  # First order basins (headwaters)
  nthOrdBas = list()
  
  nthOrdBas[[1]] = procOrd = hwBas # initialize processing order
  
  n = 2 # start the counter for indexing the loop; n = 1 addressed with headwaters
  
  # subsequent order basins
  repeat {
    
    nthOrdBas[[n]] = lnks[which(lnks$Basn %in% nthOrdBas[[n - 1]]), 2]
    
    nthOrdBas[[n]] = unique(nthOrdBas[[n]]) # remove duplicates
    
    nthOrdBas[[n]] = nthOrdBas[[n]][which(nthOrdBas[[n]] != 0)] # remove zeros

    if (length(nthOrdBas[[n]]) == 0) { # exit when the nthOrdBas has no elements
      
      break
      
    }
    
    procOrd = c(procOrd, nthOrdBas[[n]])
    
    n = n + 1      
    
  }
  
  # Once the order vector has been created, keep the LAST indexed instance of dup basins
  # The first column is the process order, the second is the basin to process in that order
  tmpOrd = data.frame(cbind('ORD' = as.vector(tapply(seq_along(procOrd), procOrd, max)),
                            'BAS' = 1 : nrow(lnks)), stringsAsFactors = FALSE)
  
  tmpOrd = tmpOrd[order(tmpOrd$ORD), ]
  
  tmpOrd$ORD = 1 : nrow(lnks)
  
  rownames(tmpOrd) = 1 : nrow(lnks)
  
  procLnks = list('pOrd' = tmpOrd, 'cBas' = usBas)
  
  return(procLnks)
    
}
