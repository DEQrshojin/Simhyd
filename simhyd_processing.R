
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

# simhyd_processing = function(data, parms) {

tmpDF = data.frame(matrix(0, nrow = nrow(data), ncol = 18))
names(tmpDF) = c('RIMX', 'SINT', 'RINR', 'OIMP',
                 'RINC', 'RRMO', 'OIRN', 'OSRN',
                 'RREC', 'RSMF', 'SSMS', 'RPOT',
                 'RSET', 'SGWS', 'RTRI', 'OMSK',
                 'OBAS', 'OTOT')

for (day in 1 : 200) { #nrow(data)) {
    
    # RIMX - 
    tmpDF[i, 1] = min(data[i, 3], parm[['PRSC']]) 
    
    # SINT - 
    tmpDF[i, 2] = min(data[i, 2], tmpDF[i, 1]) 
    
    # RINR - 
    tmpDF[i, 3] = data[i, 2] - tmpDF[i, 2] 
    
    # RIMP - 
    tmpDF[i, 4] = data[i, 2] - tmpDF[i, 2] 
    
    # RINC - 
    if(day == 1) {tmpDF[i, 5] = parm[['PINC']] * exp(-parm[['PINS']] * 0 / parm[['PSMS']])}
    else {tmpDF[i, 5] = parm[['PINC']] * exp(-parm[['PINS']] * 0 / parm[['PSMS']])}
    
    # RRMO - 
    
    
    
    
    
    
}
    





# }