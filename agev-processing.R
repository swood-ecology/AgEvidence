#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)

#### SET WORKING DIRECTORY ####

setwd(funr::get_script_path())


source('processing-functions.R')

#### READ DATA ####
# Normative effects files
ne_mod_3 <- read_excel("data/10Mar20_Normative_effects_groups_modified_2.xlsx")

# Data files
cc <- read_excel("data/Covercrops_AgEvidence.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_AgEvidence.xlsx", sheet = "Results")
pm <- read_excel("data/PestMgmt_AgEvidence.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_AgEvidence.xlsx", sheet = "Results")


#### MANIPULATE DATA ####
# Create lists for filtering out
filtered_rv_units <- c("^#$", "(arcsine)", "log10")
filtered_finelevel_group <- c("knife_knife",
                              "unfertilized_plant",
                              "unfertilized_split",
                              "variable_variable",
                              "band_injection",
                              "injection_injection",
                              "placement_pointinjection_knifeinjection",
                              "surfaceband_belowsurface",
                              "split_preplantV6_plant_V6")

# Filter data files before data checking
cc <- cc  %>%
  filter(!rv_units %in% filtered_rv_units)

nm <- nm  %>%
  filter(!rv_units %in% filtered_rv_units) %>%
  filter(!finelevel_group %in% filtered_finelevel_group)

till <- till %>%
  filter(!rv_units %in% filtered_rv_units)

pm <- pm %>%
  filter(!rv_units %in% filtered_rv_units)

#### GL1 RENAMING ####
pm <- pm %>%
  mutate(review = "Pest Management")
ne_mod_3 <- ne.mods(ne_mod_3) 
  
#### GL2 RENAMING ####
cc <- gl2.rename(cc) 
till <- gl2.rename(till)
nm <- gl2.rename(nm)
pm <- gl2.rename(pm)

#### GL3 RENAMING ####
cc <- gl3.rename(cc) 
till <- gl3.rename(till)
nm <- gl3.rename(nm)
pm <- gl3.rename(pm)

#### GENERATE NEW COLUMNS FOR GL AND NE ####
# Combine the two spellings of the cover crop review categories
ne_mod_3 <- ne_mod_3 %>%
  mutate(Review=
           ifelse(Review=="Cover Crops",
                  "Cover crop",
                  Review))

# Create new columns by calling grouping() function
cc <- cc %>% 
  full_join(ne_mod_3 %>%
              filter(Review =="Cover crop")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

till <- till %>% 
  full_join(ne_mod_3 %>%
              filter(Review=="Tillage")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

nm <- nm %>% 
  full_join(ne_mod_3 %>%
              filter(Review=="Nutrient Management")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

pm <- pm %>% 
  full_join(ne_mod_3 %>%
              filter(Review=="Early Season Pest Management")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

write.csv(cc, paste0("filtered-data/Covercrops_AgEvidence_",Sys.Date(),".csv"))
write.csv(till, paste0("filtered-data/Tillage_AgEvidence_",Sys.Date(),".csv"))
write.csv(nm, paste0("filtered-data/NutrientMgmt_AgEvidence_",Sys.Date(),".csv"))
write.csv(pm, paste0("filtered-data/PestMgmt_AgEvidence_",Sys.Date(),".csv"))

