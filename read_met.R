#' Read rainfall and PET data from a csv
#'
#' @param prcFile The filepath to the precipitation data.
#' @param petFile The filepath to the PET data.
#' @return A list of data frames of \code{prcFile} and \code{petFile}.
#' @examples
#' read_met('file/path/to/precip_data.csv', 'file/path/to/pet_data.csv')

read_met = function(prcFile, petFile) {

  # Read PET
  prc = read.csv(prcFile)

  # Read PET
  pet = read.csv(petFile)

  # Rename columns to Basin IDs
  basID = paste0("B", 1 : (length(prc) - 1))

  names(prc) = c("Date", basID)

  names(pet) = c("Date", basID)

  metData = list('prc' = prc, 'pet' = pet)

  return(metData)

}
