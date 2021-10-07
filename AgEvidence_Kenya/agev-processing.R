#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
source('processing-functions.R')

#### READ DATA ####
# Normative effects file
ne <- read_excel("data/normative-effects-Kenya.xlsx")

# Data files
cc <- read_excel("data/ContinuousCover_Kenya_081721.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_Kenya_CURRENT_081721.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_Kenya_081721.xlsx", sheet = "Results")

#### MANIPULATE DATA ####
# Create lists for filtering out
filtered_rv_units <- c("^#$", "(arcsine)", "log10")

# Filter data files before data checking
cc <- cc  %>%
  filter(!is.na(trt1_value)) %>%
  filter(!rv_units %in% filtered_rv_units)

nm <- nm  %>%
  filter(!is.na(trt1_value)) %>%
  filter(!rv_units %in% filtered_rv_units)

till <- till %>%
  filter(!is.na(trt1_value)) %>%
  filter(!rv_units %in% filtered_rv_units)

# Get and write list of unique studies
cc.unique <- cc %>% select(paper_id) %>% unique()
nm.unique <- nm %>% select(paper_id) %>% unique()
till.unique <- till %>% select(paper_id) %>% unique()

write.csv(cc.unique,"spatial-data/cc-points-to-map.csv")
write.csv(nm.unique,"spatial-data/nm-points-to-map.csv")
write.csv(till.unique,"spatial-data/till-points-to-map.csv")

rm(cc.unique);rm(nm.unique);rm(till.unique)
                
#### NE RENAMING ####
ne <- ne.mods(ne) 
  
#### GL2 RENAMING ####
cc <- gl2.rename(cc) 
till <- gl2.rename(till)
nm <- gl2.rename(nm)

#### GL3 RENAMING ####
cc <- gl3.rename(cc) 
till <- gl3.rename(till)
nm <- gl3.rename(nm)

#### MARKDOWN FOR MEASURED VARIABLE AND UNITS ####
cc <- mkdown(cc) 
till <- mkdown(till)
nm <- mkdown(nm)

#### GENERATE NEW COLUMNS FOR GL AND NE ####
# Create new columns by calling grouping() function
cc <- cc %>% 
  inner_join(ne %>%
              filter(Review =="Continuous Cover") %>% 
               unique()) %>%
  select(-Review) %>%
  mutate(group_level1_alt = na_if(group_level1_alt, "NA")) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

till <- till %>% 
  left_join(ne %>% 
              filter(Review=="Tillage")) %>%
  select(-Review) %>%
  mutate(group_level1_alt = na_if(group_level1_alt, "NA")) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

nm <- nm %>% 
  left_join(ne %>%
              filter(Review=="Nutrient Amendments")) %>%
  select(-Review) %>%
  mutate(group_level1_alt = na_if(group_level1_alt, "NA")) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

#### CONVERT RATES OF CHANGE
# Avoid problems where negative rates of change create positive results
cc <- sign.correction(cc)
till <- sign.correction(till)
nm <- sign.correction(nm)


# #### REMOVE OBSERVATIONS
# # Remove observations from studies that have rates of change between negative and positive
# # Calculation % change between negative and positive creates counterintuitive results
# RV <- c("rate of soil organic carbon change in corn-corn-soybean rotation",
#         "rate of soil organic carbon change in corn-soybean rotation",
#         "annual rate of soil organic carbon change across three crop rotations",
#         "rate of soil organic carbon change across three crop rotations")
# 
# cc <- cc %>% filter(!rv %in% RV)
# till <- till %>% filter(!rv %in% RV)
# nm <- nm %>% filter(!rv %in% RV)
# pm <- pm %>% filter(!rv %in% RV)

#### WRITE FILES
write.csv(cc, paste0("filtered-data/ContinuousCover_Kenya_",Sys.Date(),".csv"))
write.csv(till, paste0("filtered-data/Tillage_Kenya_",Sys.Date(),".csv"))
write.csv(nm, paste0("filtered-data/NutrientMgmt_Kenya_",Sys.Date(),".csv"))
