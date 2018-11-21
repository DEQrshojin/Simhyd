# parm = list of hru-specific parameters
# data = data frame of Date, Precip, PET
# hru = string of the hydro response unit
# hruArea = float of the basin-hru area (ac)
# routC = vector of routing coefficients

simhyd_processing = function(hru, hruArea, parm, data, routC, usFlws) {

  options(stringsAsFactors = FALSE)

  tmpDF = data.frame(matrix(0, ncol = 19, nrow = nrow(data)))
  names(tmpDF) = c('RIMX', 'SINT', 'RINR', 'RIMP',
                   'RINC', 'RRMO', 'OIRN', 'OSRN',
                   'RREC', 'RSMF', 'RPOT', 'RSET',
                   'SSMS', 'OBAS', 'SGWS', 'RTRI',
                   'OMSK', 'OTOT', 'QCFS')

  for (i in 1 : nrow(data))
  {

    #  1. RIMX - Interception Maximum; Min(IPET[i], PRSC)
    tmpDF[i, 1] = min(data[i, 3], parm[['PRSC']])

    #  2. SINT - Interception Storage; Min(RIMX[i], IRAI[i])
    tmpDF[i, 2] = min(data[i, 2], tmpDF[i, 1])

    #  3. RINR - Interception Runoff (throughfall); IRAI[i] - SINT[i]
    tmpDF[i, 3] = data[i, 2] - tmpDF[i, 2]

    #  4. RIMP - Impervious Runoff; IRAI[i] - SINT[i] # Send straight to routing if Impervious
    tmpDF[i, 4] = data[i, 2] - tmpDF[i, 2]

    #  5. RINC - Infiltration Capacity; PINC x exp(-PINS x SSMS[i-1] / PSMS)
    if(i == 1) {
      tmpDF[i, 5] = parm[['PINC']] * exp(-parm[['PINS']] * 0 / parm[['PSMS']])
    } else {
      tmpDF[i, 5] = parm[['PINC']] *
        exp(-parm[['PINS']] * tmpDF[i - 1, 13] /parm[['PSMS']])
    }

    #  6. RRMO - Effective Infiltration; Min(RINR[i], RINC[i])
    tmpDF[i, 6] = min(tmpDF[i, 3], tmpDF[i, 5])

    #  7. OIRN - Infiltration Excess Runoff; RINR[i] - RRMO[i]
    tmpDF[i, 7] = tmpDF[i, 3] - tmpDF[i, 6]

    #  8. OSRN - Saturation Excess Runoff; PINC x (SSMS[i-1] / PSMS) x RRMO[i]
    if(i == 1) {
      tmpDF[i, 8] = parm[['PITC']] * 0 / parm[['PSMS']] * tmpDF[i, 6]
    } else {
      tmpDF[i, 8] = parm[['PITC']] * tmpDF[i - 1, 13] / parm[['PSMS']] * tmpDF[i, 6]
    }

    #  9. RREC - Groundwater Recharge; PRCH * (SSMS[i-1] / PSMS) (RRMO[i] - OSRN[i])
    if(i == 1) {
      tmpDF[i, 9] = parm[['PITC']] * (0 / parm[['PSMS']]) * (tmpDF[i, 6] - tmpDF[i, 8])
    } else {
      tmpDF[i, 9] = parm[['PITC']] * (tmpDF[i - 1, 13] / parm[['PSMS']]) *
        (tmpDF[i, 6] - tmpDF[i, 8])
    }

    # 10. RSMF - Soil Moisture Fraction; RRMO[i] - OSRN[i] - RREC[i]
    tmpDF[i, 10] = tmpDF[i, 6] - tmpDF[i, 8] - tmpDF[i, 9]

    # 11. RPOT - PET/Interception Difference; IPET[i] - SINT[i]
    tmpDF[i, 11] = data[i, 3] - tmpDF[i, 2]

    # 12. RSET - Soil Evapotranspiration; Min(10 x SSMS[i-1] / PSMS, RPOT[i])
    if(i == 1) {
      tmpDF[i, 12] = min(10 * 0 / parm[['PSMS']], tmpDF[i, 11])
    } else {
      tmpDF[i, 12] = min(10 * tmpDF[i - 1, 13] / parm[['PSMS']], tmpDF[i, 11])
    }

    # 13. SSMS - Soil Moisture Storage; SSMS[i-1] + RSMF[i] - RSET[i]
    if(i == 1) {
      tmpDF[i, 13] = 0 + tmpDF[i, 10] - tmpDF[i, 12]
    } else {
      tmpDF[i, 13] = tmpDF[i - 1, 13] + tmpDF[i, 10] - tmpDF[i, 12]
    }

    # 14. OBAS - Baseflow; PBAS x SGWS[i-1]
    if(i == 1) {
      tmpDF[i, 14] = parm[['PBAS']] * 0
    } else {
      tmpDF[i, 14] = parm[['PBAS']] * tmpDF[i - 1, 15]
    }

    # 15. SGWS - Groundwater Storage; SGWS[i-1] + RREC[i] - OBAS[i]
    if(i == 1) {
      tmpDF[i, 15] = 0 + tmpDF[i, 9] - tmpDF[i, 14]
    } else {
      tmpDF[i, 15] = tmpDF[i - 1, 15] + tmpDF[i, 9] - tmpDF[i, 14]
    }

    ######################################################
    # MUST ADD A TERM IN HERE FOR UPSTREAM LINK INFLOWS!!!
    ######################################################

    # 16. IUSF - Upstream inflows (Unrouted), from run_simhyd()
    usFlws

    # 16. RTRI - Routing Inflow; RIMP[i] --OR-- OIRN[i] + OSRN[i]
    if(hru == "IMP") {
      tmpDF[i, 16] = tmpDF[i, 4]
    } else {
      tmpDF[i, 16] = tmpDF[i, 7] + tmpDF[i, 8]
    }

    # 17. OMSK - Hydrologic Flow Routing; C_0 x RTRI[i] + C_1 x RTRI[i-1] + C_2 x SMSK[i-1]
    if(i == 1) {
      tmpDF[i, 17] = tmpDF[i, 16]
    } else {
      tmpDF[i, 17] = routC[1] * tmpDF[i, 16] + routC[2] * tmpDF[i - 1, 16] +
                     routC[3] * tmpDF[i - 1, 17]
    }

    # 18. OTOT - Total Stream Flow; OMSK[i] + OBAS[i]
    tmpDF[i, 18] = tmpDF[i, 17] + tmpDF[i, 14]

  }

  # 19. Convert to flow (cfs)
  tmpDF$QCFS = tmpDF$OTOT * # Depth of mm per day
    25.4 / 12 /  # Depth of ft per day
    hruArea *    # Volume of ac-ft per day
    43560 /      # Volume of cubic ft per day
    86400        # Volume of cubic ft per second

  outflow = data.frame(cbind('Date' = data$Date, 'QCFS' = tmpDF$QCFS))

  outflow$Date = as.POSIXct(outflow$Date, origin = "1970-01-01")

  return(outflow)

}
