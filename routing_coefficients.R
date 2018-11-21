routing_coeff = function(PSDL, PIOB, dt) {

     D = PSDL - PSDL * PIOB + 0.5 * dt
     routC = c((-PSDL * PIOB + 0.5 * dt) / D,       # C_0
               (PSDL * PIOB + 0.5 * dt) / D,        # C_1
               (PSDL - PSDL * PIOB - 0.5 * dt) / D) # C_2
     return(routC)

}

