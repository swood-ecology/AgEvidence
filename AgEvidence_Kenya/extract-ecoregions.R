library(tidyverse)
library(sf)
library(readxl)

# SETWD
setwd("/Users/stephen.wood/Box Sync/Work/The Nature Conservancy/Global Soils/AgEvidence/code-and-data/AgEvidence_Kenya")

# Add data
kenya_ecoregions <- st_read("spatial-data/Ecozone Map Layers/Kenya_Ecoregions.shp") %>% st_make_valid()
cc_points <- read_excel("data/ContinuousCover_Kenya.xlsx", sheet = "ExpD_Location")
till_points <- read_excel("data/Tillage_Kenya.xlsx", sheet = "ExpD_Location")

# Convert points to shapefiles
cc_points <- st_as_sf(cc_points, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(kenya_ecoregions))

till_points <- st_as_sf(till_points, coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(st_crs(kenya_ecoregions))

# Join with polygons to extract ecoregion information
cc <- st_join(cc_points, kenya_ecoregions)
till <- st_join(till_points, kenya_ecoregions)

# Remove unnecessary columns
cc_tb <- as_tibble(cc) %>% select(
  paper_id:trtmt_splitC_levels,ECO_CODE,ECO_NAME,WWF_MHTNUM,WWF_MHTNAM
)
till_tb <- as_tibble(till) %>% select(
  paper_id:trtmt_splitC_levels,ECO_CODE,ECO_NAME,WWF_MHTNUM,WWF_MHTNAM
)

# Write files
write.csv(cc_tb,"data/ContinuousCover_EcoRegions.csv")
write.csv(till_tb,"data/Tillage_EcoRegions.csv")

