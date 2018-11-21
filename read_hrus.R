#' Read catchment by catchment hydro response units
#'
#' @param hruFile The filepath to the precipitation data.
#' @return A list of lists of \code{hrus} areas per catchment.
#' @examples
#' read_met('file/path/to/precip_data.csv', 'file/path/to/pet_data.csv')

read_hru = function(hruFile) {

  # Read PET
  hrus = read.csv(hruFile)

  return(hrus)

}
