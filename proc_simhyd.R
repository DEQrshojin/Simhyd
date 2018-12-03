proc_simhyd <- function(area, pars, met) { # numeric, list, data frame
  
  # Based on algorithms from this website:
  # https://wiki.ewater.org.au/display/SD41/SIMHYD+with+routing+-+SRG
  
  options(stringsAsFactors = FALSE)
  tmpDF = data.frame(matrix(0, ncol = 17, nrow = nrow(met))) # UPDATE NCOL
  names(tmpDF) = c('RPRV', 'RIMV', 'SIMV', 'OIVR',
                   'SINT', 'RINR', 'RINC', 'RRMO',
                   'OIRN', 'OSRN', 'RREC', 'RSMF',
                   'RPOT', 'RSET', 'SSMS', 'OBAS',
                   'SGWS')
  for (i in 1 : nrow(met))
  {
    #  1. RPRV - Incident previous flow fraction; IPRC[i] * PPVF
    tmpDF[i, 1] = met[i, 2] * pars[['PPVF']]
    #  2. RIMV - Incident impervious flow fraction; IPRC[i] * (1 - PPVF)
    tmpDF[i, 2] = met[i, 2] * (1 - pars[['PPVF']])
    #  3. SIMV - Impervious Storage and ET; min(IPET[i], (1 - PPVF) * PIVT, RIMV[i])
    tmpDF[i, 3] = min(met[i, 3], (1 - pars[['PPVF']]) * pars[['PIVT']], tmpDF[i, 2])
    #  4. OIVR - Impervious Runoff; RIMV[i] - SIMV[i]; send to quickflow
    tmpDF[i, 4] = tmpDF[i, 2] - tmpDF[i, 3]
    #  5. SINT - Interception Storage and ET; min(IPET[i], PRSC, RPRV[i])
    tmpDF[i, 5] = min(met[i, 3], pars[['PRSC']], tmpDF[i, 1])
    #  6. RINR - Interception Runoff (throughfall); RPRV[i] - SINT[i]
    tmpDF[i, 6] = tmpDF[i, 1] - tmpDF[i, 5]
    #  7. RINC - Infiltration Capacity; PINC x exp(-PINS x SSMS[i-1] / PSMS)
    if(i == 1) {
      tmpDF[i, 7] = pars[['PINC']] * exp(-pars[['PINS']] * 0 / pars[['PSMS']])
    } else {
      tmpDF[i, 7] = pars[['PINC']] *
                    exp(-pars[['PINS']] * tmpDF[i - 1, 15] / pars[['PSMS']])
    }
    #  8. RRMO - Effective Infiltration; Min(RINR[i], RINC[i])
    tmpDF[i, 8] = min(tmpDF[i, 6], tmpDF[i, 7])
    #  9. OIRN - Infiltration Excess Runoff; RINR[i] - RRMO[i]
    tmpDF[i, 9] = tmpDF[i, 6] - tmpDF[i, 8]
    # 10. OSRN - Interflow/Saturation Excess Runoff; PINC x (SSMS[i-1] / PSMS) x RRMO[i]
    if(i == 1) {
      tmpDF[i, 10] = pars[['PITC']] * 0 / pars[['PSMS']] * tmpDF[i, 8]
    } else {
      tmpDF[i, 10] = pars[['PITC']] * tmpDF[i - 1, 15] / pars[['PSMS']] * tmpDF[i, 8]
    }
    # 11. RREC - Groundwater Recharge; PRCH * (SSMS[i-1] / PSMS) (RRMO[i] - OSRN[i])
    if(i == 1) {
      tmpDF[i, 11] = pars[['PRCH']] * (0 / pars[['PSMS']]) * (tmpDF[i, 8] - tmpDF[i, 10])
    } else {
      tmpDF[i, 11] = pars[['PRCH']] * (tmpDF[i - 1, 15] / pars[['PSMS']]) *
                     (tmpDF[i,8] - tmpDF[i, 10])
    }
    # 12. RSMF - Soil Moisture Fraction; RRMO[i] - OSRN[i] - RREC[i]
    tmpDF[i, 12] = tmpDF[i, 8] - tmpDF[i, 10] - tmpDF[i, 11]
    # 13. RPOT - PET/Interception Difference; IPET[i] - SINT[i]
    tmpDF[i, 13] = met[i, 3] - tmpDF[i, 5]
    # 14. RSET - Soil Evapotranspiration; Min(10 x SSMS[i-1] / PSMS, RPOT[i])
    if(i == 1) {
      tmpDF[i, 14] = min(10 * 0 / pars[['PSMS']], tmpDF[i, 13])
    } else {
      tmpDF[i, 14] = min(10 * tmpDF[i - 1, 15] / pars[['PSMS']], tmpDF[i, 13])
    }
    # 15. SSMS - Soil Moisture Storage; SSMS[i-1] + RSMF[i] - RSET[i]
    if(i == 1) {
      tmpDF[i, 15] = 0 + tmpDF[i, 12] - tmpDF[i, 14]
    } else {
      tmpDF[i, 15] = tmpDF[i - 1, 15] + tmpDF[i, 12] - tmpDF[i, 14]
    }
    # 16. OBAS - Baseflow; PBAS x SGWS[i-1]
    if(i == 1) {
      tmpDF[i, 16] = pars[['PBAS']] * 0
    } else {
      tmpDF[i, 16] = pars[['PBAS']] * tmpDF[i - 1, 17]
    }
    # 17. SGWS - Groundwater Storage; SGWS[i-1] + RREC[i] - OBAS[i]
    if(i == 1) {
      tmpDF[i, 17] = 0 + tmpDF[i, 11] - tmpDF[i, 16]
    } else {
      tmpDF[i, 17] = tmpDF[i - 1, 17] + tmpDF[i, 11] - tmpDF[i, 16]
    }
  }
  # Subset tmpDF for runoff: imprv (OIVR), infilt. (OIRN), sat. (OSRN), and baseflow (OBAS)
  latFlow = tmpDF[, c(4, 9, 10, 16)] # columns 1, 2, 3, 4
  # infiltration (OIRN), saturation excess (OSRN), and imperv runoff (OIVR) are "quickflow'
  latFlow$OQCK = latFlow$OIVR + latFlow$OIRN + latFlow$OSRN # column 5
  # Convert to cfs
  if (area == 0) {
    latFlow = latFlow * 0 # set to zero if catchment doesn't have any hru of that kind
  } else {
    latFlow = latFlow / 25.4 / 12 * area * 43560 / 86400 # Conversion from mm/day to cfs
  }
  latFlow$Date = met$Date
  latFlow = latFlow[, c(6, 5, 4)]
  
  return(latFlow)

}

