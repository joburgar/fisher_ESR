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
# 01_create_RE.R
# script to create range extent (Nature Serve) from Maxent output
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 3-Apr-2025
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

# Load raster files
raster_fDir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/MMP/3.Analysis_Conceptual/Maxent/"

PEPE_bioclim <- rast(paste0(raster_fDir,"PEPE_bioclim_elev_suitability.tif"))
PEPE_bec <- rast(paste0(raster_fDir,"PEPE_BEC_suitability.tif"))

# Check the stack
print(PEPE_bioclim)
print(PEPE_bec)

plot(PEPE_bioclim)
plot(PEPE_bec)


# Check values
hist(values(PEPE_bec))
hist(values(PEPE_bioclim))

# Adjust values to classes
# 0.0-0.10 = not fisher habitat
# 0.1-0.20 = unlikely fisher habitat
# 0.2-0.40 = possible fisher habitat (dispersing) 
# 0.4-0.60 = likely fisher habitat
# >0.60 = suitable fisher habitat

# Define classification matrix
reclass_matrix_bec <- matrix(c(
  0.00, 0.10, 1,  # not fisher habitat
  0.10, 0.20, 2,  # unlikely fisher habitat
  0.20, 0.40, 3,  # possible fisher habitat (dispersing)
  0.40, 0.60, 4,  # likely fisher habitat
  0.60, 1.00, 5   # suitable fisher habitat
), ncol = 3, byrow = TRUE)

reclass_matrix_bio <- matrix(c(
  0.00, 0.05, 1,  # not fisher habitat
  0.05, 0.10, 2,  # unlikely fisher habitat
  0.10, 0.20, 3,  # unlikely fisher habitat
  0.20, 0.40, 4,  # possible fisher habitat (dispersing)
  0.40, 0.60, 5,  # likely fisher habitat
  0.60, 1.00, 6   # suitable fisher habitat
), ncol = 3, byrow = TRUE)

# for Boreal bioclim (reclass2), with fewer sample locations went with a reclass  of <0.05 as not fisher habitat (1),
# for Colmbian bioclim (reclass), with more sample locations went with a reclass  of <0.15 as not fisher habitat (1),


# Apply classification
PEPE_bec_reclass <- classify(PEPE_bec, reclass_matrix_bec)
PEPE_bioclim_reclass <- classify(PEPE_bioclim, reclass_matrix_bio)

# Plot results
plot(PEPE_bec_reclass, main="Reclassified PEPE_bec")
plot(PEPE_bioclim_reclass, main="Reclassified PEPE_bioclim")

## clip to Boreal geographic area
# Load the polygon shapefile (SpatVector)
GIS_Dir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Fisher_status/CDC_ESR/"
# list.files(GIS_Dir) # check it works
Boreal_aoi <- sf::st_read(dsn = GIS_Dir, layer="Boreal_boundary")

ggplot()+
  geom_sf(data=Boreal_aoi)

# Ensure both have the same CRS
Boreal_aoi <- st_transform(Boreal_aoi, crs = st_crs(PEPE_bioclim_reclass))

# Filter to fisher region of interest
aoi_filtered <- Boreal_aoi

# Clip and mask raster to AOI
clipped_raster <- crop(PEPE_bioclim_reclass, aoi_filtered)  # Crop to bounding box
clipped_raster <- mask(clipped_raster, aoi_filtered)  # Mask to exact shape

plot(clipped_raster)

###--- Export as both rasters and polygon shapefiles
# Define file paths for output
raster_output_path <- "PEPE_bec_reclassified.tif"
polygon_output_path <- "PEPE_bec_reclassified.shp"

# Export reclassified raster
writeRaster(PEPE_bec_reclass, raster_output_path, overwrite=TRUE, datatype="INT2S")

# Convert raster to polygons (vectorize)
PEPE_bec_polygon <- as.polygons(PEPE_bec_reclass, trunc=TRUE, dissolve=TRUE)

# Export as shapefile
writeVector(PEPE_bec_polygon, polygon_output_path, overwrite=TRUE)

# Repeat for PEPE_bioclim
raster_output_path2 <- "PEPE_bioclim_reclassified.tif"
polygon_output_path2 <- "PEPE_bioclim_reclassified2.shp"
polygon_output_path_Boreal <- "PEPE_bioclim_reclass_Boreal.shp"

writeRaster(PEPE_bioclim_reclass, raster_output_path2, overwrite=TRUE, datatype="INT2S")

PEPE_bioclim_polygon <- as.polygons(PEPE_bioclim_reclass, trunc=TRUE, dissolve=TRUE)
writeVector(PEPE_bioclim_polygon, polygon_output_path2, overwrite=TRUE)

PEPE_bioclim_Boreal_polygon <- as.polygons(clipped_raster, trunc=TRUE, dissolve=TRUE)
writeVector(PEPE_bioclim_Boreal_polygon, polygon_output_path_Boreal, overwrite=TRUE)

######################################
###--- check to summarise the RE area
PEPE_bioclim_Boreal_sf <- st_as_sf(PEPE_bioclim_Boreal_polygon)

PEPE_bioclim_Boreal_sf <- PEPE_bioclim_Boreal_sf %>% mutate(area=st_area(.))
PEPE_bioclim_Boreal_sf %>% filter(PEPE_bioclim_elev_suitability != 1) %>% summarise(sum(area)) %>% st_drop_geometry()


# Break down the one large multipolygon into individual polygons and remove smallest ones
# Explode it into separate polygons
# Boreal_main <- PEPE_bioclim_Boreal_polygon %>%
#   st_cast("POLYGON") %>%
#   mutate(area = st_area(.)) %>%
#   filter(as.numeric(area) > 1e6)  # Only keep polygons larger than 10 million m²
#   
# st_write(Boreal_main, "PEPE_bioclim_Boreal_polygon2.shp", driver = "ESRI Shapefile", overwrite = TRUE)


