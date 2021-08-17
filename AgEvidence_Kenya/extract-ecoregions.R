library(tidyverse)
library(sf)
library(readxl)

# SETWD
setwd("/Users/stephen.wood/Box Sync/Work/The Nature Conservancy/Global Soils/AgEvidence/code-and-data/AgEvidence_Kenya")

# Add data
kenya_ecoregions <- st_read("spatial-data/Ecozone Map Layers/Kenya_Ecoregions.shp", 
                            crs = 3857) %>% st_make_valid()
kenya_farming_systems <- st_read("spatial-data/FAO_Farming-Systems_KE/fao_farming-systems_ke-larger-border.shp", 
                                 crs = 3857) %>% st_make_valid()
cc_points <- read_excel("data/ContinuousCover_Kenya.xlsx", sheet = "ExpD_Location")
till_points <- read_excel("data/Tillage_Kenya.xlsx", sheet = "ExpD_Location")

# Convert points to shapefiles
cc_points <- st_as_sf(cc_points, coords = c("longitude", "latitude"), crs = 3857) 
till_points <- st_as_sf(till_points, coords = c("longitude", "latitude"), crs = 3857)

# Join with polygons to extract ecoregion information
cc <- st_join(cc_points, kenya_farming_systems)
till <- st_join(till_points, kenya_farming_systems)

# Remove unnecessary columns
cc_tb <- as_tibble(cc) %>% select(
  paper_id:trtmt_splitC_levels,DESCRIPTIO
)
till_tb <- as_tibble(till) %>% select(
  paper_id:trtmt_splitC_levels,DESCRIPTIO
)

# Write files
write.csv(cc_tb,"data/ContinuousCover_EcoRegions.csv")
write.csv(till_tb,"data/Tillage_EcoRegions.csv")
