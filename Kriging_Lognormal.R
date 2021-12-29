# Kriging_Lognormal.R - Perform lognormal kriging
# Run Data_Prep.R first!

###############################################
# Variogram estimation
radon2_centroids_sp = as_Spatial(radon2_centroids) # Convert ot SP object
evgm = variogram(logGM~1, data=radon2_centroids_sp, width = 10, cutoff=190, cloud=F) # Compute empirical semivariogram
plot(evgm, main = "Empirical Semivariogram for GM", pch = 19, cex = 1) # Plot empirical semivariogram

# Fit parametric varigram using Matern model with nugget
vgm_model = vgm(psill=0.5, model="Mat", range=40, nugget=0.05, kappa=1)
vgm_est = fit.variogram(evgm, vgm_model, fit.kappa=TRUE)

# Plot fitted variogram model
plot(evgm, model=vgm_est)
plot(evgm, model=vgm_est, xlab = 'Distance', ylab = 'Semivariance',
     main = "Matern Semivariogram for GM", pch = 19, cex = 1) # pl=T to add the number of pairs

png(file="Plots\\Kriging_Lognormal\\Semivariogram.png",
    width=800, height=340)
plot(evgm, model=vgm_est)
plot(evgm, model=vgm_est, xlab = 'Distance', ylab = 'Semivariance',
     main = "Matern Semivariogram for GM", pch = 19, cex = 1) # pl=T to add the number of pairs
dev.off()


radon2_centroids = st_transform(radon2_centroids, crs = 4269) # Adjust coordinate system to allow switch to SP
radon2_centroids_sp = as_Spatial(radon2_centroids) # Convert to SP

# Perform Ordinary Kriging on log trnasformed data
# Make grid for prediction locations. Function defined in Data_Prep.R
ohiogrid = make_ohiogrid(cellsize = 0.02) 
ohiogrid_sp = as_Spatial(ohiogrid)
#ok_log_ohiogrid = krige(logGM~1, locations=radon2_centroids_sp, newdata=ohiogrid_sp, model=vgm_est)
#save('ok_log_ohiogrid', file='Data\\RData\\Kriging_Lognormal\\ok_log_ohiogrid.RData')
load(file='Data\\RData\\Kriging_Lognormal\\ok_log_ohiogrid.RData')

ohiogrid_sp$logGM = (ok_log_ohiogrid@data)[,1] # merge kriging predictions into grid
ohiogrid_sp$logGM.var = (ok_log_ohiogrid@data)[,2] # merge kriging variances into grid

# Backtransform from logGM to GM
ohiogrid_sp$GM_bt = exp(ohiogrid_sp$logGM + (ohiogrid_sp$logGM.var / 2) - mean(radon2$logGM))
mu_bt = mean(ohiogrid_sp$GM_bt)
mu_original = mean(radon2$GM)
ohiogrid_sp$GM = ohiogrid_sp$GM_bt * (mu_original/mu_bt)

#Plot Lognormal kriging
plot.range_sec2 = range(c(ohiogrid_sp$GM, ohiogrid_sp$GM))
plot_ok_log = ggplot() + # Use ap lain theme
  geom_tile(data = as.data.frame(ohiogrid_sp), #Draw grid
            aes(coords.x1, coords.x2, fill=GM)) + # Colour with lnlead
  scale_fill_gradient(low = "#F7E9E9", high = "#180505") + # Color scale
  coord_fixed(ratio=69/54.6)+ # fix aspect ratio
  xlab(element_blank()) + ylab(element_blank()) + # axes labels
  theme_bw() + ggtitle("Lognormal Kriging Estimates for GM")+
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank())
plot_ok_log

ggsave(filename = 'LognormalKrig.png', plot = last_plot(), device = 'png', path = 'Plots\\Kriging_Lognormal',
       scale = 1.5, width = 2.35, height = 2.15, units = "in")


#Plot Lognormal kriging, binary version
ohiogrid_sp$GM_GE4 = ohiogrid_sp$GM >= 4
plot_ok_log_GE4 = ggplot() + # Use ap lain theme
  geom_tile(data = as.data.frame(ohiogrid_sp), #Draw grid
            aes(coords.x1, coords.x2, fill=GM_GE4)) + # Colour with lnlead
  scale_fill_manual(values = c("#C6C2C2", "black")) +
  coord_fixed(ratio=69/54.6) + # fix aspect ratio
  xlab(element_blank()) + ylab(element_blank()) + # axes labels
  theme_bw() + ggtitle("Lognormal Kriging for GM\nValues >= 4pCi/L")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank()) +
  theme(legend.position = "none")
plot_ok_log_GE4
ggsave(filename = 'LognormalKrig_Binary.png', plot = last_plot(), device = 'png', path = 'Plots\\Kriging_Lognormal',
       scale = 1.5, width = 2.35, height = 2.45, units = "in")


