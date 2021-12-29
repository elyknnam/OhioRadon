# LAGP_Lognormal.R - Perform local gaussian process approximation for GM using Lognormal approach
# Run Data_Prep.R first!

library(laGP)

# Make grid for prediction locations. Function defined in Data_Prep.R
ohiogrid = make_ohiogrid(cellsize = 0.1)
ohiogrid_sp = as_Spatial(ohiogrid) # Convert to SP

# Make local gaussian process approximation predictions
formals(aGP)[c("X", "Z", "XX")] = list(as.matrix(coordinates(radon2_centroids_sp)),  # Use cooreinates as input
                                       radon2_centroids_sp$logGM, # use logGM as output
                                       as.matrix(coordinates(ohiogrid_sp)))
formals(aGP)["verb"] = 1
#laGP_log_mspe = aGP(d=list(mle = FALSE, start = 6), method = 'mspe')
#save('laGP_log_mspe', file='Data\\RData\\LAGP_Lognormal\\laGP_log_mspe.RData')
load(file='Data\\RData\\LAGP_Lognormal\\laGP_log_mspe.RData')

ohiogrid_sp$logGM = laGP_log_mspe$mean # predicted values
ohiogrid_sp$logGM.var = laGP_log_mspe$var # predicted variance
# back transform, same approach as lognormal kriging
ohiogrid_sp$GM_bt = exp(ohiogrid_sp$logGM + (ohiogrid_sp$logGM.var / 2) - mean(radon2$logGM))
mu_bt = mean(ohiogrid_sp$GM_bt)
mu_original = mean(radon2$GM)
ohiogrid_sp$GM = ohiogrid_sp$GM_bt * (mu_original/mu_bt) # predicted values


# Plot Predictions
plot.range_sec2 = range(c(ohiogrid_sp$GM, ohiogrid_sp$GM))
plot_laGP_log = ggplot() + # Use ap lain theme
  geom_tile(data = as.data.frame(ohiogrid_sp), #Draw grid
            aes(coords.x1, coords.x2, fill=GM)) + # Colour with lnlead
  scale_fill_gradient(low = "#F7E9E9", high = "#180505") + # Color scale
  coord_fixed(ratio=69/54.6)+ # fix aspect ratio
  xlab(element_blank()) + ylab(element_blank()) + # axes labels
  theme_bw() + ggtitle("laGP Estimates for GM\nLognormal Method")+
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank())
plot_laGP_log

ggsave(filename = 'laGP_log.png', plot = last_plot(), device = 'png', path = 'Plots\\LAGP_Lognormal',
       scale = 1.5, width = 2.35, height = 2.3, units = "in")

