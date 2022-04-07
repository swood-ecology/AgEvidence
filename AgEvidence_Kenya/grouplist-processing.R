#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#test

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
library(plyr)


# Data files
cc <- read_excel("data/ContinuousCover_Kenya_081721.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_Kenya_CURRENT_081721.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_Kenya_081721.xlsx", sheet="Results")


# Generate nested grouping lists
cc_groups <- cc %>% 
  select(group_level1, group_level2, group_level3) %>% unique
nm_groups <- nm %>% 
  select(group_level1, group_level2, group_level3) %>% unique
till_groups <- till %>% 
  select(group_level1, group_level2, group_level3) %>% unique # grouping, normative_effect

group_list1 <- full_join(cc_groups, nm_groups)
group_lista <- full_join(group_list1, till_groups)


# Export csv
write.csv(group_lista, paste0("filtered-data/grouplists_Kenya_",Sys.Date(),".csv"))

write.csv(till_groups, paste0("filtered-data/tilllists_Kenya_",Sys.Date(),".csv"))

cc <- read_excel("data/ContinuousCover_Kenya_081721.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_Kenya_CURRENT_081721.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_Kenya_081721.xlsx", sheet="Results")

# Generate list of group discrepancies
unique(unlist(strsplit(as.character(cc$mgmt_intention), ";")))
unique(unlist(strsplit(as.character(nm$group_level2), ";")))
unique(unlist(strsplit(as.character(till$group_level3), ";")))

#Generate list of unique crops
# Data files - Cash crop tab
cc_crops <- read_excel("data/ContinuousCover_Kenya_081721.xlsx", sheet = "CashCrop")
nm_crops <- read_excel("data/NutrientMgmt_Kenya_CURRENT_081721.xlsx", sheet = "CashCrop")
till_crops <- read_excel("data/Tillage_Kenya_081721.xlsx", sheet="CashCrop")

cc_cropslist <- unique(unlist(strsplit(as.character(cc_crops$control_species), ";")))
nm_cropslist <- unique(unlist(strsplit(as.character(nm_crops$control_species), ";")))
till_cropslist <- unique(unlist(strsplit(as.character(till_crops$cash_species), ";")))

crops <- unique(c(as.character(cc_cropslist), as.character(nm_cropslist), as.character(till_cropslist)))
cropslist <- as.data.frame(crops)

# Export csv
write.csv(cropslist, paste0("filtered-data/cropslist_Kenya_",Sys.Date(),".csv"))
          