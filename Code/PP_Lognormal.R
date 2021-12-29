# PP_Lognormal.R - Perform predictive process for GM using lognormal approach
# Run Data_Prep.R first!

library(spBayes)

# # Optional code to run model on only subset of data due to time constraint
#radon2_centroids_sp_small = radon2_centroids_sp[runif(dim(radon2)[1]) < 0.2, ]
#dim(radon2_centroids_sp_small)

# Create grid of knots
knotgrid <- radon2_centroids %>%
  st_make_grid(cellsize = 0.4, what = "centers") %>%
  st_intersection(state_shp) %>%
  st_sf()
knotgrid = as_Spatial(knotgrid)
knotgrid = as.data.frame(coordinates(knotgrid))
names(knotgrid) = c('x', 'y')

# Run MCMC sampling for model parameters
n.samples=3000
#m.pp_log = spLM(logGM~1, data = radon2_centroids_sp,
#                coords=coordinates(radon2_centroids_sp),
#                starting = list("phi"=0.9,"sigma.sq"=3, "tau.sq"=1.5),
#                tuning = list("phi"=0.05, "sigma.sq"=0.05, "tau.sq"=0.02),
#                priors = list("phi.Unif"=c(0.1, 1),
#                              "sigma.sq.IG"=c(2, 1),
#                              "tau.sq.IG"=c(2, 1)),
#                cov.model="exponential",
#                n.samples=n.samples, verbose=TRUE, n.report=250,
#                knots=as.matrix(knotgrid))
#names(m.pp_log)

#save('m.pp_log', file='Data\\RData\\PP_Lognormal\\m.pp_log.RData')
load(file='Data\\RData\\PP_Lognormal\\m.pp_log.RData')


# recover beta and spatial random effects
burn.in = 0.4*n.samples
#m.pp_log.recover = spRecover(m.pp_log, start=burn.in, verbose=T)
#save('m.pp_log.recover', file='Data\\RData\\PP_Lognormal\\m.pp_log.recover.RData')
load(file='Data\\RData\\PP_Lognormal\\m.pp_log.recover.RData')

# CIs for model parameters
round(summary(m.pp_log.recover$p.theta.recover.samples)$quantiles[,c(3,1,5)],3)
# CIs for Mean 
round(summary(m.pp_log.recover$p.beta.recover.samples)$quantiles[c(3,1,5)],3)

# Samples for model parameters
for (i in seq(1, 3)) {
  parname = dimnames(m.pp_log.recover$p.theta.recover.samples)[[2]][i]
  plot(m.pp_log.recover$p.theta.recover.samples[,i], main=parname)
}
# Samples for Mean
plot(m.pp_log.recover$p.beta.recover.samples, main=paste('beta',i,sep=" "))


# Make predictions
ohiogrid = make_ohiogrid(cellsize = 0.06)
ohiogrid_sp = as_Spatial(ohiogrid)

test = coordinates(ohiogrid_sp)
#m.pp_log.pred.te <- spPredict(m.pp_log.recover, 
#                              pred.covars=cbind(rep(as.double(1), dim(test)[1])), 
#                              pred.coords=test,
#                              start=0.96*n.samples,verbose=T)

#save('m.pp_log.pred.te', file='Data\\RData\\PP_Lognormal\\m.pp_log.pred.te.RData')
load(file='Data\\RData\\PP_Lognormal\\m.pp_log.pred.te.RData')

# Get predictions, using mean of MCMC predictions for each point
pp_pred_means = apply(m.pp_log.pred.te[[1]], 1, function(x) {mean(x)})
pp_pred_var = apply(m.pp_log.pred.te[[1]], 1, function(x) {var(x)})
length(pp_pred_means)
pp_pred_means_df = data.frame(logGM = pp_pred_means, 
                              logGM.var = pp_pred_var, 
                              x = as.data.frame(coordinates(ohiogrid_sp))$coords.x1,
                              y = as.data.frame(coordinates(ohiogrid_sp))$coords.x2)
# back transform
pp_pred_means_df$GM_bt = exp(pp_pred_means_df$logGM + (pp_pred_means_df$logGM.var / 2) - mean(radon2$logGM))
mu_bt = mean(pp_pred_means_df$GM_bt)
mu_original = mean(radon2$GM)
pp_pred_means_df$GM = pp_pred_means_df$GM_bt * (mu_original/mu_bt)


# Plot predictions
plot.range_sec2 = range(c(pp_pred_means_df$GM, pp_pred_means_df$GM))
plot_PPspBayes = ggplot() + # Use ap lain theme
  geom_tile(data = pp_pred_means_df, #Draw grid
            aes(x, y, fill=GM)) + # Colour with lnlead
  scale_fill_gradient(low = "#F7E9E9", high = "#180505") + # Color scale
  coord_fixed(ratio=69/54.6)+ # fix aspect ratio
  xlab(element_blank()) + ylab(element_blank()) + # axes labels
  theme_bw() + ggtitle("Predictive Process Estimates for GM\nLognormal Method")+
  theme(plot.title = element_text(hjust = 0.5, size=13)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank()) +
  geom_point(knotgrid, mapping = aes(x=x, y=y), color = '#D50101')
plot_PPspBayes

ggsave(filename = 'PP_log_spBayes.png', plot = last_plot(), device = 'png', path = 'Plots\\PP_Lognormal',
       scale = 1.5, width = 2.35, height = 2.15, units = "in")





