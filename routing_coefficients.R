routing_coefficients = function(rte, dt) {

     rte = inputs[["rte"]]
     rte$PDSL = rte$PDSL + 1
     rte$PIOB = rte$PIOB - 0.1
      
     # D = k(1 - x) + 0.5dt
     rte$D = rte$PDSL * (1 - rte$PIOB) + 0.5 * dt
     rte$C_0 = (-rte$PDSL * rte$PIOB + 0.5 * dt) / rte$D 
     rte$C_1 = (rte$PDSL * rte$PIOB + 0.5 * dt) / rte$D
     rte$C_2 = (rte$PDSL * (1 - rte$PIOB) - 0.5 * dt) / rte$D

     return(rte)

}


