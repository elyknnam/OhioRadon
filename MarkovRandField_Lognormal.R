# MarkovRandField.R - Predict GM using Markov random field
# Run Data_Prep.R first!

library(gamlss.spatial)
library(mgcv)

# Convert coordinates to list format - data that does not include population
radon2_coord = list()
for (i in 1:dim(radon2)[1]) {
  radon2_coord[[as.character(i)]] = st_coordinates(radon2$geometry[i])
}

# Convert coordinates to list format - data that includes population
radon2_withpop_coord = list()
for (i in 1:dim(radon2_withpop)[1]) {
  radon2_withpop_coord[[as.character(i)]] = st_coordinates(radon2_withpop$geometry[i])
}


# Neighors & precision matrix

radon2_nb=polys2nb(radon2_coord) # Get zip code neighbor structure from coordinates
radon2$row_id = as.factor(row.names(radon2)) # Assign IDs
precisionC = nb2prec(radon2_nb, x=radon2$row_id) # Compute precision matrix based on neighbor structure

radon2_withpop_nb=polys2nb(radon2_withpop_coord) # Get zip code neighbor structure from coordinates
radon2_withpop$row_id = as.factor(row.names(radon2_withpop)) # Assign IDs
precisionC_withpop = nb2prec(radon2_withpop_nb, x=radon2_withpop$row_id) # Compute precision matrix based on neighbor structure


# Fit Gaussian Markov Random Field
mrf_Nweight_GM_log = MRFA(radon2$logGM, radon2$row_id, 
                          neighbour=radon2_nb, weights = radon2$N)
mrf_ZPOPweight_GM_log = MRFA(radon2_withpop$logGM, radon2_withpop$row_id, 
                             neighbour=radon2_withpop_nb, weights = radon2_withpop$ZPOP)

logGM = predict(mrf_Nweight_GM_log) # predicted values
logGM = predict(mrf_ZPOPweight_GM_log) # predicted values

# back transform
GM_bt = exp(logGM)
mu_bt = mean(GM_bt)
mu_original = mean(radon2$GM)
GM = GM_bt * (mu_original/mu_bt)
radon2$mrf_Nweight_GM_log = GM

# back transform
GM_bt = exp(logGM)
mu_bt = mean(GM_bt)
mu_original = mean(radon2$GM)
GM = GM_bt * (mu_original/mu_bt)
radon2_withpop$mrf_ZPOPweight_GM_log = GM

# Plot predictions
plot_row_lattice2 = function(ds, outname, titletext, ...) {
  mrf_plot = ggplot() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank()) +
    geom_sf(data = ds, aes(...), colour = NA) + 
    scale_fill_gradient(limits = c(0,13), low = "#F7E9E9", high = "#180505", na.value = '#D50101') + # Color scale
    ggtitle(titletext) +
    theme(plot.title = element_text(hjust = 0.5, size=13)) +
    theme(legend.title = element_blank()) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "in"))
  print(mrf_plot)
  ggsave(filename = outname, plot = last_plot(), device = 'png', path = 'Plots\\MarkovRandField_Lognormal',
         scale = 1.5, width = 2.45, height = 2.3, units = "in")
}

plot_row_lattice2(ds = radon2, outname = 'mrf_Nweight_GM_log.png',
                  titletext = 'Markov Random Field\nWeighted by N\nLognormal Approach', 
                  fill = mrf_Nweight_GM_log)
plot_row_lattice2(ds = radon2_withpop, outname = 'mrf_ZPOPweight_GM_log.png',
                  titletext = 'Markov Random Field\nWeighted by 2010 Census Pop\nLognormal Approach', 
                  fill = mrf_ZPOPweight_GM_log)



#Plot masks cut off at 4 pCi/L
radon2$GM_GE4 = radon2$GM > 4
radon2$mrf_Nweight_GM_log_GE4 = radon2$mrf_Nweight_GM_log > 4
radon2_withpop$mrf_ZPOPweight_GM_log_GE4 = radon2_withpop$mrf_ZPOPweight_GM_log > 4


# Plot predictions
plot_row_lattice3 = function(ds, outname, titletext, ...) {
  ggplot() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank()) +
    geom_sf(data = ds, aes(...), colour = NA) + 
    scale_fill_manual(values = c("#C6C2C2", "black")) + 
    theme(legend.position = "none") +
    ggtitle(titletext) +
    theme(plot.title = element_text(hjust = 0.5, size=13)) +
    theme(legend.title = element_blank()) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "in"))
  
  ggsave(filename = outname, plot = last_plot(), device = 'png', path = 'Plots\\MarkovRandField_Lognormal',
         scale = 1.5, width = 2.35, height = 2.45, units = "in")
}

plot_row_lattice3(ds = radon2, outname = 'raw_GM_13cut.png', 
                  titletext = 'Raw GM Radon\nValues >= 4pCi/L', fill = GM_GE4)
plot_row_lattice3(ds = radon2, outname = 'mrf_Nweight_GM.png',
                  titletext = 'Markov Random Field\nWeighted by N\nValues >= 4pCi/L', 
                  fill = mrf_Nweight_GM_log_GE4)
plot_row_lattice3(ds = radon2_withpop, outname = 'mrf_ZPOPweight_GM.png',
                  titletext = 'Markov Random Field\nWeighted by 2010 Census Pop\nValues >= 4pCi/L', 
                  fill = mrf_ZPOPweight_GM_log_GE4)



