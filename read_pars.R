#' Read hydrologic parameters from a csv
#'
#' @param parFile The filepath to the HRU parameters csv.
#' @return A list of HRU specific \code{pars} parameter characteristics.
#' @examples
#' read_pars('file/path/to/parameters.csv')

read_pars <- function(parFile) {
  
  options(stringsAsFactors = FALSE)

  par = read.csv(parFile)

  hruNames = colnames(par)

  hruNames = hruNames[5 : length(hruNames)]

  hruPar = list()

  pars = list()

  # Parse out each parameter into a list
  for (i in 5 : length(par)) {    # iterate through each hru

    for (j in 1 : nrow(par)) {    # iterate through each parameter

      hruPar[[j]] = par[j, i]     # by each parameter

      names(hruPar)[j] = par[j, 4]#

    }

    pars[[i - 4]] = hruPar

    names(pars)[i - 4] = hruNames[i - 4]

  }

  return(pars)

}
