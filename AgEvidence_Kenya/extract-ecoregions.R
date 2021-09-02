library(tidyverse)
library(sf)
library(readxl)

# SETWD
setwd("/Users/stephen.wood/Box Sync/Work/The Nature Conservancy/Global Soils/AgEvidence/code-and-data/AgEvidence_Kenya")

# Define variables
cc.name <- "data/ContinuousCover_Kenya_081721.xlsx"
till.name <- "data/Tillage_Kenya_081721.xlsx"
nm.name <- "data/NutrientMgmt_Kenya_CURRENT_081721.xlsx"

# Read in data filters
cc_to_map <- read_csv("spatial-data/cc-points-to-map.csv", 
                             col_types = cols(X1 = col_skip()))
nm_to_map <- read_csv("spatial-data/nm-points-to-map.csv", 
                      col_types = cols(X1 = col_skip()))
till_to_map <- read_csv("spatial-data/till-points-to-map.csv", 
                      col_types = cols(X1 = col_skip()))

# Add data
kenya_ecoregions <- st_read("spatial-data/Ecozone Map Layers/Kenya_Ecoregions.shp", 
                            crs = 3857) %>% st_make_valid()
kenya_farming_systems <- st_read("spatial-data/FAO_Farming-Systems_KE/fao_farming-systems_ke_GADM.shp", 
                                 crs = 3857) %>% st_make_valid()
cc_points <- read_excel(cc.name, sheet = "ExpD_Location")
till_points <- read_excel(till.name, sheet = "ExpD_Location")
nm_points <- read_excel(nm.name, sheet = "ExpD_Location")

# Filter points by studies that have data
cc_points <- cc_points %>% filter( (paper_id %in% cc_to_map$paper_id) ) 
till_points <- till_points %>% filter( (paper_id %in% till_to_map$paper_id) )
nm_points <- nm_points %>% filter( (paper_id %in% nm_to_map$paper_id) )
      
# Convert points to shapefiles
cc_points <- st_as_sf(cc_points, coords = c("longitude", "latitude"), crs = 3857) 
till_points <- st_as_sf(till_points, coords = c("longitude", "latitude"), crs = 3857)
nm_points <- st_as_sf(nm_points, coords = c("longitude", "latitude"), crs = 3857)

# Join with polygons to extract ecoregion information
cc <- st_join(cc_points, kenya_farming_systems)
till <- st_join(till_points, kenya_farming_systems)
nm <- st_join(nm_points, kenya_farming_systems)

# Remove unnecessary columns
cc_tb <- as_tibble(cc) %>% select(
  paper_id:trtmt_splitC_levels,DESCRIPTIO
)
till_tb <- as_tibble(till) %>% select(
  paper_id:trtmt_splitC_levels,DESCRIPTIO
)
nm_tb <- as_tibble(nm) %>% select(
  paper_id:trtmt_splitC_levels,DESCRIPTIO
)

# Write files
write.csv(cc_tb,"data/ContinuousCover_EcoRegions.csv")
write.csv(till_tb,"data/Tillage_EcoRegions.csv")
write.csv(nm_tb,"data/NutrientManagement_EcoRegions.csv")
