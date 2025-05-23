#####################################################################################
# 03_create_elev.R
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
raster_fDir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Projects/Fishers - General_Status_etc/"

PEPE_elev <- rast(paste0(raster_fDir,"PEPE_elev.tif"))

# Check the raster
print(PEPE_elev)
plot(PEPE_elev)


# Check values
hist(values(PEPE_elev))

# Adjust values to classes
# Define classification matrix
reclass_matrix_elev <- matrix(c(
  -94, 0, 9,  # not fisher habitat
  0, 1000, 1,  # fisher habitat
  1001, 1100, 2,  # possible fisher habitat
  1101, 1200, 3,  # possible fisher habitat
  1201, 1300, 4,  # possible fisher habitat
  1301, 1400, 5,  # possible fisher habitat
  1401, 1500, 6,  # possible fisher habitat
  1501, 1600, 7,  # possible fisher habitat
  1601, 1700, 8,  # possible fisher habitat
  1701, 4069, 9  # not fisher habitat
), ncol = 3, byrow = TRUE)


# Define classification matrix
reclass_matrix_elev <- matrix(c(
  -Inf,     0,     9,  # not fisher habitat
  0,  1001,     1,  # fisher habitat (0 to 1000 inclusive)
  1001,  1101,     2,
  1101,  1201,     3,
  1201,  1301,     4,
  1301,  1401,     5,
  1401,  1501,     6,
  1501,  1601,     7,
  1601,  1701,     8,
  1701,  Inf,      9   # not fisher habitat
), ncol = 3, byrow = TRUE)


# Reclassify the raster
PEPE_elev_classified <- classify(PEPE_elev, rcl = reclass_matrix_elev)

# exclude non fisher habitat
PEPE_elev_classified[PEPE_elev_classified == 9] <- NA

# Plot to check
plot(PEPE_elev_classified)

# writeRaster(PEPE_elev_classified, "PEPE_elev_classified.tif", overwrite = TRUE)

## clip to geographic area
# Load the polygon shapefile (SpatVector)
GIS_Dir <- "//sfp.idir.bcgov/S140/S40203/Ecosystems/Conservation Science/Species/Mesocarnivores/Fisher_status/CDC_ESR/"
# list.files(GIS_Dir) # check it works
Columbian_aoi <- sf::st_read(dsn = GIS_Dir, layer="Columbian_boundary")

ggplot()+
  geom_sf(data=Columbian_aoi)

# Ensure both have the same CRS
Columbian_aoi <- st_transform(Columbian_aoi, crs = st_crs(PEPE_elev_classified))

# Filter to fisher region of interest
aoi_filtered <- Columbian_aoi

# Clip and mask raster to AOI
clipped_raster <- crop(PEPE_elev_classified, aoi_filtered)  # Crop to bounding box
clipped_raster <- mask(clipped_raster, aoi_filtered)  # Mask to exact shape

plot(clipped_raster)

###--- Export as both rasters and polygon shapefiles
# Define file paths for output
raster_output_path <- "PEPE_elev_reclassified.tif"
polygon_output_path <- "PEPE_elev_reclassified.shp"

# Export reclassified raster
writeRaster(clipped_raster, raster_output_path, overwrite=TRUE, datatype="INT2S")

# Convert raster to polygons (vectorize)
PEPE_elev_polygon <- as.polygons(clipped_raster, trunc=TRUE, dissolve=TRUE)

# Export as shapefile
writeVector(PEPE_elev_polygon, polygon_output_path, overwrite=TRUE)

