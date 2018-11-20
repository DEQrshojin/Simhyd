# SIMHYD MODEL WITH RIVER ROUTING

# INPUT	                    UN  TYPE	VARS    ORD. EQ.
# Potential Evapotranspiration  mm	INPUT	IPET    -
# Precipitation 	            mm	INPUT	IRAI    -

# PROCESSES (Time dependent)
# Interception Maximum	        mm	PROCESS	RIMX    1   = Min(IPET[i], PRSC)
# Interception Storage	        mm	STORAGE	SINT    2   = Min(RIMX[i], IRAI[i])
# Interception Runoff	        mm	PROCESS	RINR    3a  = IRAI[i] - SINT[i]
# Impervious Runoff             mm  OUTPUT  OIMP    3b  = IRAI[i] - SINT[i] if HRU = Impervious
# Infiltration Capacity 	    mm	PROCESS	RINC    4   = PINC x exp(-PINS x SSMS[i-1] / PSMS)
# Effective Infiltration        mm	PROCESS	RRMO    5   = Min(RINR[i], RINC[i])
# Infiltration Excess Runoff 	mm	OUTPUT	OIRN    6   = RINR[i] - RRMO[i]
# Saturation Excess Runoff 	    mm	OUTPUT	OSRN    7   = PINC x (SSMS[i-1] / PSMS) x RRMO[i]
# Groundwater Recharge	        mm	PROCESS	RREC    8   = PRCH * (SSMS[i-1] / PSMS) (RRMO[i] - OSRN[i])
# Soil Moisture Fraction    		PROCESS	RSMF    9   = RRMO[i] - OSRN[i] - RREC[i]
# Soil Moisture Storage	        mm	STORAGE	SSMS   10   = SSMS[i-1] + RSMF[i] - RSET[i]
# PET/Interception Difference 	mm	PROCESS	RPOT   11   = IPET[i] - SINT[i]
# Soil Evapotranspiration	    mm	PROCESS	RSET   12   = Min(10 x SSMS[i-1] / PSMS, RPOT[i])
# Groundwater Storage	        mm	STORAGE	SGWS   13   = SGWS[i-1] + RREC[i] - OBAS[i]
# Routing Inflow                mm  PROCESS RTRI   14   = OIRN[i] + OSRN[i] + OIMP[i]
# Hydrologic Flow Routing       mm  OUTPUT  OMSK   14   = C_0 x RTRI[i] + C_1 x RTRI[i-1] + C_2 x SMSK[i-1]
# Baseflow 	                    mm	OUTPUT	OBAS   15   = PBAS x SGWS[i-1]
# Total Stream Flow             mm  OUTPUT  OTOT   16   = OMSK[i] + OBAS[i]

# PARAMETERS (Time independent)
# Baseflow coeff		            PARAMTR	PBAS
# Infiltration coeff 	        mm	PARAMTR	PINC
# Infiltration shape		        PARAMTR	PINS
# Interflow coeff		            PARAMTR	PITC
# Recharge coeff	    	        PARAMTR	PRCH
# Rainfall Interception         mm	PARAMTR	PRSC
# Soil Moisture Storage         mm	PARAMTR	PSMS
# Surface Delay (Routing)       day PARAMTR PSDL    
# Inflow to Outflow Bias            PARAMTR PIOB
# Time step                     day PARAMTR dt

# ROUTING COEFFICIENTS (finite difference)
# D     = PSDL - PSDL x PIOB + 0.5dt
# C_0   = (-PSDL x PIOB + 0.5dt) / D
# C_1   = (PSDL x PIOB + 0.5dt) / D
# C_2   = (PSDL - PSDL x PIOB + 0.5dt) / D



# FLOW ROUTING ----
# PSDL = 2 # days
# PIOB = 0.2
# dt = 1 # days
# D     = PSDL - PSDL * PIOB + 0.5 * dt
# C_0   = (-PSDL * PIOB + 0.5 * dt) / D
# C_1   = (PSDL * PIOB + 0.5 * dt) / D
# C_2   = (PSDL - PSDL * PIOB - 0.5 * dt) / D
# 
# check = C_0 + C_1 + C_2
# 
# qdf = data.frame(cbind('TIME' = c(1 : 20),
#                        'INFL' = c(4, 7, 11, 17, 22, 27, 30, 28, 25, 23,
#                                  20, 17, 14, 11, 8, 5, 4, 4, 4, 4),
#                        'OTFL' = rep(0, 20)),
#                  stringsAsFactors = FALSE)
# 
# for (i in 1 : nrow(qdf)) {
#     if(i == 1) {qdf[i, 3] = qdf[i, 2]}
#     else {qdf[i, 3] = C_0 * qdf[i, 2] + C_1 * qdf[i - 1, 2] + C_2 * qdf[i - 1, 3]}
# }
