#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
source('processing-functions.R')

#### READ DATA ####
# Normative effects file
ne <- read_excel("data/normative-effects.xlsx")

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

#### NE RENAMING ####
ne <- ne.mods(ne) 
  
#### GL1 RENAMING ####
cc <- cc %>%
  mutate(review="Cover Crops")

till <- till %>%
  mutate(
    group_level1 =
      ifelse(rv == "soil methane oxidation (CH4-C) in in April" | rv == "soil methane oxidation (CH4-C) in in August" |
               rv == "water extractable organic carbon (0-5 cm)" | rv == "water extractable organic carbon (5-20 cm)" |
               rv == "active carbon concentration" | rv == "maximum mineralizable soil carbon (Michaelis-Menten equation)" |
               rv == "mineralizable soil carbon" | rv == "natural abundance of 13-C in soil (delta 13-C)" | 
               rv == "delta 13 C in soil",
             "Other Soil Properties",
             group_level1)
  )

cc <- cc %>%
  mutate(
  group_level1 =
    ifelse(rv == "water extractable organic carbon (0-5 cm)" | rv == "water extractable organic carbon (5-20 cm)" |
             rv == "active carbon concentration" | rv == "maximum mineralizable soil carbon (Michaelis-Menten equation)" |
           rv == "mineralizable soil carbon" | rv == "natural abudance of 13-C in soil (delta 13-C)" | rv == "delta 13 C in soil",
           "Other Soil Properties",
           group_level1)
)
  

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
# Create new columns by calling grouping() function
cc <- cc %>% 
  full_join(ne %>%
              filter(Review =="Cover Crops")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

till <- till %>% 
  full_join(ne %>%
              filter(Review=="Tillage")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

nm <- nm %>% 
  full_join(ne %>%
              filter(Review=="Nutrient Management")) %>%
  select(-NOTES,-Review) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

pm <- pm %>% 
  full_join(ne %>%
              filter(Review=="Pest Management")) %>%
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
write.csv(ne, paste0("filtered-data/NormativeEffects_AgEvidence_",Sys.Date(),".csv"))

