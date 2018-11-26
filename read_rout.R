read_rout <- function(rteFile) {
     
  rte = read.csv(rteFile)
  
  return(rte)

}

# library(raster)
# 
# shpFile = inFiles[6]
# 
# shpFile = shapefile(shpFile)
# 
# lnks = data.frame(cbind('Basn' = as.numeric(shpFile@data[["HSPF_Bas"]]),
#                         'DSBs' = as.numeric(shpFile@data[["DS_Basin"]])),
#                   stringsAsFactors = FALSE)