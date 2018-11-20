datPath = 'E:/R/siletz/001_data/'
parFile = paste0(datPath, 'par.csv')
prcFile = paste0(datPath, 'p.csv')
petFile = paste0(datPath, 'pet.csv')
flwFile = paste0(datPath, 'q.csv')

inputs = read_simhyd_inputs(parFile = parFile,
                            prcFile = prcFile,
                            petFile = petFile,
                            flwFile = flwFile)

parm = inputs[[1]]
data = inputs[[2]]

outflow = simhyd_processing(parms, data)
