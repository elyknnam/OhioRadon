# ExploratoryAnalysis.R - Exploratory analysis
# Run Data_Prep.R first!

setwd('C:\\Users\\Buddy\\Desktop\\SpatialStatistics\\Final')

# Read in data
radon1 = read.csv("Data\\RadonZip.csv")

#####################################################
# Univariate analysis

# Histogram function
make_hist = function(ds, xlab, bins = 200, ...) {
  hist_plot = ggplot(ds, aes(...)) +
    geom_histogram(bins = bins) +
    ylab("# of Zip Codes") + xlab(xlab) +
    ggtitle(paste('Distribution of', xlab)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11, margin = margin(t = 0, r = 12, b = 0, l = 0)),
          plot.title = element_text(size=13, margin = margin(t = 0, r = 0, b = 8, l = 0))) + 
    theme(axis.text = element_text(size=9)) 
  return(hist_plot)
}

# Quantiles for N
quantile(radon2$N, c(0, 0.1, 0.2, 0.25, 0.5, 0.75, 1))

# Histogram of N
hist_N = make_hist(ds = radon2, xlab='N', x = N)
hist_N
ggsave(filename = 'hist_n.png', plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
       scale = 1.5, width = 5, height = 1.25, units = "in")

# Quantiles for Mean
quantile(radon2$Mean, c(0, 0.1, 0.2, 0.25, 0.5, 0.75, 1))

# Histogram of Mean
hist_Mean = make_hist(ds = radon2, xlab='Mean', x = Mean)
hist_Mean
ggsave(filename = 'hist_mean.png', plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
       scale = 1.5, width = 5, height = 1.25, units = "in")

# Quantiles for GM
quantile(radon2$GM, c(0, 0.1, 0.2, 0.25, 0.5, 0.75, 1))

# Histogram of GM
hist_GM = make_hist(ds = radon2, xlab='GM', x = GM)
hist_GM
ggsave(filename = 'hist_GM.png', plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
       scale = 1.5, width = 5, height = 1.25, units = "in")

# Histogram of log(GM)
hist_logGM = make_hist(ds = radon2, xlab='log(GM)', bins = 100, x = logGM)
hist_logGM
ggsave(filename = 'hist_logGM.png', plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
       scale = 1.5, width = 5, height = 1.25, units = "in")



##############################################

# Make bivariate scatter plot
make_scatter = function(ds, xlab, ylab, title, ...) {
  scatter_plot = ggplot() + # Use ap lain theme
    geom_point(data = ds, aes(...)) + 
    xlab(element_blank()) + ylab(element_blank()) + # axes labels
    theme_bw() + ggtitle(title)+
    ylab(ylab) + xlab(xlab) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.x = element_text(size=11),
          axis.title.y = element_text(size=11, margin = margin(t = 0, r = 12, b = 0, l = 0)),
          plot.title = element_text(size=13, margin = margin(t = 0, r = 0, b = 8, l = 0))) +
    theme(strip.text = element_text(face = 'bold', size = 14)) + 
    theme(axis.text = element_text(size=9)) +
    theme(legend.position = "none")
  return(scatter_plot)
}

# Plot scatter of N vs GM
scatter_N_GM = make_scatter(ds = radon2, xlab = 'N', ylab = 'GM', title = 'N vs GM', x=N, y=GM)
scatter_N_GM
ggsave(filename = 'scatter_N_GM.png', plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
       scale = 1.5, width = 5, height = 1.5, units = "in")

# Plot scatter of ZPOP vs logGM
scatter_ZPOP_GM = make_scatter(ds = radon2_withpop, xlab = '2010 Census Pop.', ylab = 'GM', 
                               title = '2010 Census Pop. vs GM', x=ZPOP, y=GM)
scatter_ZPOP_GM
ggsave(filename = 'scatter_ZPOP_GM.png', plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
       scale = 1.5, width = 5, height = 1.5, units = "in")


##############################################

# Plot raw lattice
plot_row_lattice = function(ds, outname, titletext, ...) {
  lattice_plot = ggplot() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank()) +
    geom_sf(data = ds, aes(...), colour = NA) + 
    scale_fill_gradient(low = "#F7E9E9", high = "#180505") + # Color scale
    ggtitle(titletext) +
    theme(plot.title = element_text(hjust = 0.5, size=13)) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "in"))
  print(lattice_plot)
  ggsave(filename = outname, plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
         scale = 1.5, width = 2.3, height = 2.35, units = "in")
}

plot_row_lattice(ds = radon2, outname = 'lattice_mean.png', 
                 titletext = 'Mean Radon by Zip Code', fill = Mean)
plot_row_lattice(ds = radon2, outname = 'lattice_GM.png', 
                 titletext = 'GM Radon by Zip Code', fill = GM)
plot_row_lattice(ds = radon2[radon2$N >= 10,], outname = 'lattice_GM_GE10.png', 
                 titletext = 'GM Radon by Zip Code, N>=10', fill = GM)
plot_row_lattice(ds = radon2[radon2$N >= 50,], outname = 'lattice_GM_GE50.png', 
                 titletext = 'GM Radon by Zip Code, N>=50', fill = GM)



# Plot raw lattice, cut off at threshold & colored red
plot_row_lattice_threshold = function(ds, outname, titletext, ...) {
  lattice_plot = ggplot() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank()) +
    geom_sf(data = ds, aes(...), colour = NA) + 
    scale_fill_gradient(limits = c(0,13), low = "#F7E9E9", high = "#180505", na.value = '#D50101') + # Color scale
    ggtitle(titletext) +
    theme(plot.title = element_text(hjust = 0.5, size=13)) +
    theme(legend.title = element_blank()) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "in"))
  print(lattice_plot)
  ggsave(filename = outname, plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
         scale = 1.5, width = 2.3, height = 2.35, units = "in")
}

plot_row_lattice_threshold(ds = radon2, outname = 'raw_GM_13cut.png', 
                  titletext = 'GM Radon\nScale Cut off at 13', fill = GM)


##############################################

# Scatter plot map, size by N or population


make_scatter_map = function(ds, outname, title, ...) {
  scatter_map = ggplot() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.text=element_blank(), axis.ticks=element_blank()) +
    geom_sf(data = state_shp) + 
    geom_sf(data = ds, aes(colour = GM, ...)) +
    #scale_colour_gradient(low = "#E7D8D8", high = "#180505") +# Color scale
    scale_colour_gradient(limits = c(0,13), low = "#F7E9E9", high = "#180505", na.value = '#D50101') + # Color scale
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size=13)) +
    theme(plot.margin=grid::unit(c(0,0,0,0), "in"))
  print(scatter_map)
  ggsave(filename = outname, plot = last_plot(), device = 'png', path = 'Plots\\ExploratoryAnalysis',
         scale = 1.5, width = 2.65, height = 2.7, units = "in")
}
make_scatter_map(ds = radon2_centroids, outname = 'scatter_map_N.png', 
                 title = 'GM Radon\nSize by N\nScale Cut off at 13', size = N)
make_scatter_map(ds = radon2_centroids_withpop, outname = 'scatter_map_ZPOP.png', 
                 title = 'GM Radon\nSize by 2010 Census Pop.\nScale Cut off at 13', size = ZPOP)


