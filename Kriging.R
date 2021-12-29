# Kriging.R - Perform ordinary kriging
# Run Data_Prep.R first!

###############################################
# Variogram estimation
radon2_centroids_sp = as_Spatial(radon2_centroids) # Convert ot SP object
evgm = variogram(GM~1, data=radon2_centroids_sp, width = 10, cutoff=190, cloud=F) # Compute empirical semivariogram
plot(evgm, main = "Empirical Semivariogram for GM", pch = 19, cex = 1) # Plot empirical semivariogram

# Fit parametric varigram using Matern model with nugget
vgm_model = vgm(psill=0.5, model="Mat", range=40, nugget=0.05, kappa=1)
vgm_est = fit.variogram(evgm, vgm_model, fit.kappa=TRUE)

# Plot fitted variogram model
plot(evgm, model=vgm_est)
plot(evgm, model=vgm_est, xlab = 'Distance', ylab = 'Semivariance',
     main = "Matern Semivariogram for GM", pch = 19, cex = 1) # pl=T to add the number of pairs

png(file="Plots\\Kriging\\Semivariogram.png",
    width=800, height=340)
plot(evgm, model=vgm_est)
plot(evgm, model=vgm_est, xlab = 'Distance', ylab = 'Semivariance',
     main = "Matern Semivariogram for GM", pch = 19, cex = 1) # pl=T to add the number of pairs
dev.off()


radon2_centroids = st_transform(radon2_centroids, crs = 4269) # Adjust coordinate system to allow switch to SP
radon2_centroids_sp = as_Spatial(radon2_centroids) # Convert to SP

# Perform Ordinary Kriging
# Make grid for prediction locations. Function defined in Data_Prep.R
ohiogrid = make_ohiogrid(cellsize = 0.02) # Create grid for predictions
ohiogrid_sp = as_Spatial(ohiogrid) # Convert to SP
#ok_ohiogrid = krige(GM~1, locations=radon2_centroids_sp, newdata=ohiogrid_sp, model=vgm_est)
#save('ok_ohiogrid', file='Data\\RData\\Kriging\\ok_ohiogrid.RData')
load(file='Data\\RData\\Kriging\\ok_ohiogrid.RData')

ohiogrid_sp$GM = (ok_ohiogrid@data)[,1] # merge kriging predictions into grid
ohiogrid_sp$GM_var = (ok_ohiogrid@data)[,2] # merge kriging variances into grid

#Plot Ordinary kriging
plot.range_sec2 = range(c(ohiogrid_sp$GM, ohiogrid_sp$GM))
pl.pred.lnlead_sec2 = ggplot() + # Use ap lain theme
  geom_tile(data = as.data.frame(ohiogrid_sp), #Draw grid
            aes(coords.x1, coords.x2, fill=GM)) + # Colour with lnlead
  scale_fill_gradient(low = "#F7E9E9", high = "#180505") + # Color scale
  coord_fixed(ratio=69/54.6)+ # fix aspect ratio
  xlab(element_blank()) + ylab(element_blank()) + # axes labels
  theme_bw() + ggtitle("Ordinary Kriging Estimates for GM")+
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank())
pl.pred.lnlead_sec2

ggsave(filename = 'OK.png', plot = last_plot(), device = 'png', path = 'Plots\\Kriging',
       scale = 1.5, width = 2.3, height = 2.15, units = "in")

