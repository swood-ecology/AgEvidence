library(tidyverse)
library(sf)
library(readxl)

#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

# Define variables
cc_web <- read.csv("filtered-data/ContinuousCover_Kenya_2022-11-18.csv")
till_web <- read.csv("filtered-data/Tillage_Kenya_2022-11-18.csv")
nm_web <- read.csv("filtered-data/NutrientMgmt_Kenya_2022-11-18.csv")

# Read in data filters
cc_to_map <- cc_web %>% select(paper_id) %>% unique
till_to_map <- till_web %>% select(paper_id) %>% unique
nm_to_map <- nm_web %>% select(paper_id) %>% unique

#cc_to_map <- read_csv("spatial-data/cc-points-to-map.csv", 
                             #col_types = cols(X1 = col_skip()))
#nm_to_map <- read_csv("spatial-data/nm-points-to-map.csv", 
 #                     col_types = cols(X1 = col_skip()))
#till_to_map <- read_csv("spatial-data/till-points-to-map.csv", 
 #                     col_types = cols(X1 = col_skip()))

# Add data
# kenya_ecoregions <- st_read("spatial-data/Ecozone Map Layers/Kenya_Ecoregions.shp", 
#                             crs = 3857) %>% st_make_valid()
kenya_farming_systems <- st_read("spatial-data/FAO_Farming-Systems_KE/fao_farming-systems_ke_GADM.shp", 
                                 crs = 3857) %>% st_make_valid()
cc_points <- read.csv("data/ContinuousCover/ContinuousCover_Kenya_complete_ExpD.csv")
till_points <- read.csv("data/Tillage/Tillage_Kenya_complete_ExpD.csv")
nm_points <- read.csv("data/NutrientMgmt/NutrientMgmt_Kenya_complete_ExpD.csv")


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

#Rename Coastal artisanal fishing to Coastal

nm_tb$DESCRIPTIO <- ifelse(nm_tb$DESCRIPTIO == "Coastal artisanal fishing",
                           "Coastal", nm_tb$DESCRIPTIO)
cc_tb$DESCRIPTIO <- ifelse(cc_tb$DESCRIPTIO == "Coastal artisanal fishing",
                           "Coastal", cc_tb$DESCRIPTIO)
till_tb$DESCRIPTIO <- ifelse(till_tb$DESCRIPTIO == "Coastal artisanal fishing",
                           "Coastal", till_tb$DESCRIPTIO)

# Write files
write.csv(cc_tb,"data/ContinuousCover_FarmingSystems.csv", col.names=FALSE)
write.csv(till_tb,"data/Tillage_FarmingSystems.csv", col.names=FALSE)
write.csv(nm_tb,"data/NutrientManagement_FarmingSystems.csv", col.names=FALSE)
