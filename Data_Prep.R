# Data_Prep.R - Prepare Data
# Run this program before running any other program!!!

setwd('C:\\Users\\Buddy\\Desktop\\SpatialStatistics\\Final')


library(sf)
library(dplyr)
library(tmap)
library(gstat)
library(sp)
library(tidyr)
library(ggplot2)
library(rgeos)
library(rmapshaper)

# Read in radon data
radon1 = read.csv("Data\\RadonZip.csv")
radon1$ZCTA5 = radon1$Zip.Code # Rename zip code variable
radon1$zipcode_int = as.integer(as.character(radon1$ZCTA5)) # convert to integer zip code

# Read in ZCTA shape files
zipcode_geo_full = st_read("Data\\Shapefile\\tl_2020_us_zcta520")
zipcode_geo_full$zipcode_int = as.integer(as.character(zipcode_geo_full$ZCTA5CE20)) # Convert ZCTA5 to integer zip code
zipcode_geo_full$ZCTA5 = zipcode_geo_full$ZCTA5CE20 # Rename ZCTA5 column
# Subset shapefile to only include Ohio zip codes (between 43001 and 45999)
zipcode_geo_full = zipcode_geo_full[43001 <= zipcode_geo_full$zipcode_int & zipcode_geo_full$zipcode_int <= 45999,] 

# Simplify the shapefile polygon boundaries so that plotting is quicker
#zipcode_geo = rmapshaper::ms_simplify(input = as(zipcode_geo_full, 'Spatial')) %>%
#  st_as_sf()
#save('zipcode_geo', file='zipcode_geo_simple.RData')
load(file='Data\\RData\\zipcode_geo_simple.RData')

# Load the ZCTA to County correspondance file. 
# This file includes the 2010 census populations for each ZCTA.
ZCTA_pop = read.csv("Data\\ZCTAtoCounty.csv")
# Restrict to Ohio (state code 39) and only keep the ZCTA code, state and 2010 census population (ZPOP)
ZCTA_pop = ZCTA_pop[,c('ZCTA5', 'STATE', 'ZPOP')][ZCTA_pop$STATE == 39, ] 
ZCTA_pop$zipcode_int = ZCTA_pop$ZCTA5 # Convert ZCTA to integer zipcode
ZCTA_pop = distinct(ZCTA_pop) # Drop duplicate rows

# Merge the ZCTA shapefile with the 2010 census populations
zipcode_geo_withpop = merge(zipcode_geo, ZCTA_pop, by='zipcode_int')

# Merge the radon data with shape files
radon2 = merge(zipcode_geo, radon1, by = 'zipcode_int') # version that does not include population
radon2_withpop = merge(zipcode_geo_withpop, radon1, by = 'zipcode_int') # version that includes population

# Inspect data dimensions after merge
dim(radon1)
dim(radon2)
dim(radon2_withpop)

# Inspect N for the zip codes that were dropped in the merge
# Radon data for the zip codes dropped in the merge
not_covered = radon1[sapply(radon1$zipcode_int, function(x) !(x %in% radon2_withpop$zipcode_int)), ]
sum(radon1$N) # total N in original data
sum(not_covered$N) # total N post merges
paste0(as.character(round(100*sum(not_covered$N)/sum(radon1$N), 1)), '%') # % of total N dropped


# Log transforms
radon2$logMean = log(radon2$Mean)
radon2$logGM = log(radon2$GM)
radon2$logPlusGM = log(1 + radon2$GM)
radon2_withpop$logMean = log(radon2_withpop$Mean)
radon2_withpop$logGM = log(radon2_withpop$GM)
radon2_withpop$logPlusGM = log(1 + radon2_withpop$GM)

# Compute centroids
radon2_centroids = st_centroid(radon2)
radon2_centroids_withpop = st_centroid(radon2_withpop)


##################################################################

# Function to make a grid of points across all of Ohio
# The cellsize parameter controls the grid resolution
make_ohiogrid = function(cellsize = 0.06) {
  return(radon2_centroids %>%
           st_make_grid(cellsize = cellsize, what = "centers") %>%
           st_intersection(state_shp) %>%
           st_sf())
}
