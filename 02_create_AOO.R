#####################################################################################
# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
#
#####################################################################################

#####################################################################################
# 02_create_AOO.R
# script to create AOO (Nature Serve) from FLEX output and grid of range extent
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 28-Feb-2025
#####################################################################################
R_version <- paste0("R-",version$major,".",version$minor)
.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive

# Load Packages
list.of.packages <- c("tidyverse","sf","terra")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

# Load raster and polygon shapefile

# List all raster files in a folder
r_files <- list.files(here::here(),pattern = "\\.tif$", full.names = TRUE)

# Load raster and vector shapefile
r1 <- rast(r_files[8])  # Load the first raster

# Convert raster to binary (non-zero areas)
r_bin <- r1 > 0

# Convert binary raster to polygons (terra format)
r_poly <- as.polygons(r_bin, dissolve=TRUE)

# Convert `terra` polygon to `sf` for compatibility
r_poly_sf <- st_as_sf(r_poly)
r_poly_sf <- r_poly_sf %>% filter(Omineca_stack==1)

ggplot()+
  geom_sf(data=r_poly_sf)

# Save selected polygons
getwd()
st_write(r_poly_sf, "Omineca_stack_poly.shp", driver = "ESRI Shapefile", overwrite = TRUE)

# Perform intersection using `st_intersects`
p_selected <- p[st_intersects(p, r_poly_sf, sparse = FALSE), ]

p <- st_read(dsn = here::here(), layer = "Boreal_AOO_4km")  # Load polygons
p$Id <- 1  # Ensure the polygons have an ID column

# Ensure CRS matches
crs_r <- crs(r1)
crs_p <- st_crs(p)

if (crs_r != crs_p) {
  p <- st_transform(p, crs_r)  # Reproject polygons to match raster
}


# Plot for visualization
plot(r1)
plot(st_geometry(p_selected), add=TRUE, col="red")


ggplot()+
  geom_sf(data = p)

