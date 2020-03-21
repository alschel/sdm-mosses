# SDM mosses

library(dplyr)
library(ggplot2)
library(viridis)
library(sf)
library(raster)
library(stars)
library(mapview)

# ================
# 1. Preprocessing
# ================

# Read bias file (файл коррекции ошибки)
bias <- raster("data/bias_file/bias_tetraphis.asc")
plot(bias %>% st_as_stars())

# 1.1. Localities

# Read data on species localities
localities <- read.csv("data/Species_test2.csv", stringsAsFactors = F)

# The number of localitites by species
localities %>% 
  group_by(Species) %>% 
  count() %>%
  ggplot(aes(x = reorder(Species, n), y = n))+
  geom_col()+
  scale_x_discrete(name = "Species")+
  scale_y_continuous(breaks = seq(0, 750, 50))+
  coord_flip()
  
# Create sf object from localities 
localities_sp <- 
  sp::SpatialPointsDataFrame(coords = data.frame(lon = localities$Longitude, 
                                               lat = localities$Latitude), 
                             data = localities %>% dplyr::select(Species), 
                             proj4string = crs(bias))   # use the crs description from bias file

# Let's take a look at the localities location
localities_sp %>% 
  mapview()

# Filter localities which are within bias area (those points which correpond to non-NA values of bias raster)
localities_sp <- localities_sp[!is.na(extract(bias, localities_sp)), ] # 2687 points out of 3043 initial ones

# Check all the points now are really within the bias area
plot(bias, axes=F, box=F, legend = F)
points(localities_sp, pch = 4)

# 1.2. Predictors

# Create vector of files
predictors_files <- list.files(path = "data/predictors_asc/", pattern = ".asc$")

# Stack files and mask by bias raster (у меня на этом шаге минуты 3 выполнялось, не спеши останавливать)
predictors_stack <- stack(paste0("data/predictors_asc/", predictors_files)) %>% 
  mask(bias)

# Plot one layer to check whether the mask operation was successful
predictors_stack$modcf_intraannualsd.tiff.tif %>% 
  st_as_stars() %>% 
  plot(col = viridis(30),
       main = 'modcf intraannual sd')

# =======================
# 2. Explorative analysis
# =======================

###########
# If you want filter SpatialPointsDataFrame by Species:
localities_sp[localities_sp$Species == 'Bartramiopsis_lescuiri', ]
# or
localities_sp %>% 
  st_as_sf() %>%                                  # convert to sf 
  filter(Species == 'Bartramiopsis_lescuiri') %>% # filter
  as('Spatial')                                   # convert back to sp

# If you want to extract coordinates:
coordinates(localities_sp) # returns matrix
# or 
localities_sp %>% st_as_sf() %>% st_coordinates() # returns matrix

# =======================
# 3. Test analysis
# =======================