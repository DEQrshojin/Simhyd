calib_FDC = function(calData, fdcLim) {
  
  library(ggplot2)

  # Initialize percentages vector
  flwDur = data.frame('PCT' = seq(from = fdcPar[2], to = fdcPar[1], by = fdcPar[2]) / 100)
  
  # Calculate percentiles
  flwDur$MDL_Q = quantile(calDat$MDL_Q, flwDur$PCT)
  
  flwDur$GGE_Q = quantile(calDat$GGE_Q, flwDur$PCT)
  
  # FOR NOW USE RMSE FOR COMPARISON OF FDC
  fdcRMSE = rmse(flwDur$GGE_Q, flwDur$MDL_Q)
  
  # Reshape for graphing
  flwDurP = melt(flwDur, id.vars = 'PCT')
  
  fdcPlot = ggplot(data = flwDurP) +
            geom_line(aes(x = PCT, y = value, group = variable, color = variable), size = 1.1) + 
            scale_y_log10(limits = c(10, 40000)) + 
            xlab("PCT") + ylab("Flow (cfs)") +
            theme_bw() + theme(legend.position = c(0.2, 0.8),
                               panel.grid.minor=element_blank(),
                               axis.title.x = element_blank(),
                               axis.text.x = element_text(size = 13),
                               axis.title.y = element_text(size = 13),
                               axis.text.y = element_text(size = 13),
                               plot.title = element_text(size = 13, hjust = 0.5))
  
  ggsave(filename = 'fdc_plot.png', plot = fdcPlot, path = datPath, width = 7.5,
         height = 10, dpi = 300, units = 'in')
  
  return(fdcRMSE)
  
}
