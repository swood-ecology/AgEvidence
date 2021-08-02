# Create shapefile of Kenya Ecoregions

library(tidyverse)
library(sf)

# Read shapefile of Global Ecoregions
## Source: https://geospatial.tnc.org/datasets/TNC::terrestrial-ecoregions/about
# Also, some geometries are "not valid". Correct this.
## Ref: https://pro.arcgis.com/en/pro-app/latest/help/data/validating-data/invalid-geometry.htm
global_ecoregions <- st_read("inputs/Terrestrial_Ecoregions/Terrestrial_Ecoregions.shp") %>% st_make_valid()
# Check that CRS matches what's needed for Mapbox (ESPG 3857)
st_crs(global_ecoregions)

# Read in Kenya shapefile to use for cropping and transform to match Ecoregion and Mapbox CRS
## Source: https://data.humdata.org/dataset/ken-administrative-boundaries
kenya <- st_read("inputs/kenya_shapefile/ken_admbnda_adm0_iebc_20191031.shp") %>%
  st_transform(st_crs(global_ecoregions))
# Verify resulting CRS
st_crs(kenya)

# Crop Ecoregion layer to just Kenya
## Drop attributes for kenya shapefile since all we care about is the geometry
## Recast geometry type to Multipolygon for consistency
kenya_ecoregions <- st_intersection(global_ecoregions, st_geometry(kenya)) %>% st_cast()

# Plot results of two potential layers
plot(kenya_ecoregions['ECO_NAME'])
plot(kenya_ecoregions['WWF_MHTNAM'])

# Export Kenya Ecoregion shapefile
st_write(kenya_ecoregions, "outputs/Kenya_Ecoregions/Kenya_Ecoregions.shp", append = F)

