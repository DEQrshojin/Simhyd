#' Read the catchment links from a shapefile
#'
#' @param shpFile The filepath to the precipitation data.
#' @return A list of data frames of \code{shpFile}
#' @examples
#' read_link('file/path/to/catchments.shp')

read_lnks = function(shpFile) {

  library(raster)

  shpFile = shapefile(shpFile)

  lnks = data.frame(cbind('Basn' = as.numeric(shpFile@data[["HSPF_Bas"]]),
                          'DSBs' = as.numeric(shpFile@data[["DS_Basin"]])),
                    stringsAsFactors = FALSE)

  return(lnks)

}
