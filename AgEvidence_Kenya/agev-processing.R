#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
source('processing-functions.R')

#### READ DATA ####
# Normative effects file
ne <- read_excel("data/normative-effects-Kenya.xlsx", sheet = "grouplists_Kenya")

# Data files
#cc <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
#nm <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
#till <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet = "Results")

#Import Csv format ####

cc <- read.csv("data/ContinuousCover/ContinuousCover_Kenya_complete_Results.csv")
nm <- read.csv("data/NutrientMgmt/NutrientMgmt_Kenya_complete_Results.csv")
till <- read.csv("data/Tillage/Tillage_Kenya_complete_Results.csv")


#### MANIPULATE DATA ####


# Create lists for filtering out
filtered_rv_units <- c("^#$", "(arcsine)", "log10")

# Filter data files before data checking
cc <- cc_web  %>%
  filter(!is.na(trt1_value)) %>%
  filter(!rv_units %in% filtered_rv_units)

nm <- nm_web  %>%
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


##Reload complete files to work with####
# Data files
cc <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet = "Results")


####Remove unwanted trtmt comparisons for webtool version#####
# CC remove alley crop, intercrop, monocrop-monocrop from website version

cc_web <- cc %>%
  filter(trt1_name != "alley crop", 
         trt1_name != "intercrop") %>% #drops 200 rows
  mutate(trt_namescombo = paste(trt1_name, " ",trt2_name)) %>%
  filter(trt_namescombo != "monocrop   monocrop") %>%
  select(-trt_namescombo)


# NM remove comparisons where organics are listed first and mineral fertilizers compared to mineral ferts"
nm_web <- nm %>%
  filter(trt1_name != "Animal Manure", 
         trt1_name != "Green Manure",
         trt1_name != "Incinerated Organics",
         trt1_name != "Insect Frass",
         trt1_name != "Integrated Fertility Management",  
         trt1_name != "Legume Intercrop",
         trt1_name != "Micronutrient Fertilizer",
         trt1_name != "Organic Amendment Combination",
         trt1_name != "K Mineral Fertilizer" | trt2_name != "PK Mineral Fertilizer",
         trt1_name != "N Mineral Fertilizer" | trt2_name != "N Mineral Fertilizer",
         trt1_name != "N Mineral Fertilizer" | trt2_name != "NP Mineral Fertilizer",
         trt1_name != "NK Mineral Fertilizer" | trt2_name != "NP Mineral Fertilizer",
         trt1_name != "NK Mineral Fertilizer" | trt2_name != "NPK Mineral Fertilizer",
         trt1_name != "NP Mineral Fertilizer" | trt2_name != "N Mineral Fertilizer",
         trt1_name != "NP Mineral Fertilizer" | trt2_name != "NP Mineral Fertilizer",
         trt1_name != "NP Mineral Fertilizer" | trt2_name != "NPK Mineral Fertilizer",
         trt1_name != "NPK Mineral Fertilizer" | trt2_name != "NPK Mineral Fertilizer",
         trt1_name != "P Mineral Fertilizer" | trt2_name != "NP Mineral Fertilizer",
         trt1_name != "PK Mineral Fertilizer" | trt2_name != "NK Mineral Fertilizer",
         trt1_name != "PK Mineral Fertilizer" | trt2_name != "NP Mineral Fertilizer",
         trt1_name != "PK Mineral Fertilizer" | trt2_name != "NPK Mineral Fertilizer"
         )

till_web <- till

trtlist <- nm_web %>% select(trt1_name, trt2_name) %>% unique
write.csv(trtlist, "nm_trtlist.csv", row.names = FALSE)

  
###Renaming of groups applied to both web and complete versions
              
#### NE RENAMING ####
ne <- ne.mods(ne) 
  
#### GL2 RENAMING ####
cc <- gl2.rename(cc) 
till <- gl2.rename(till)
nm <- gl2.rename(nm)

cc_web <- gl2.rename(cc_web) 
till_web <- gl2.rename(till_web)
nm_web <- gl2.rename(nm_web)

#### GL3 RENAMING ####
cc <- gl3.rename(cc) 
till <- gl3.rename(till)
nm <- gl3.rename(nm)

cc_web <- gl3.rename(cc_web) 
till_web <- gl3.rename(till_web)
nm_web <- gl3.rename(nm_web)

#### MARKDOWN FOR MEASURED VARIABLE AND UNITS ####
cc <- mkdown(cc) 
till <- mkdown(till)
nm <- mkdown(nm)

cc_web <- mkdown(cc_web) 
till_web <- mkdown(till_web)
nm_web <- mkdown(nm_web)

#### GENERATE NEW COLUMNS FOR GL AND NE ####
# Create new columns by calling grouping() function

#complete
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

###web
cc_web <- cc_web %>% 
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

till_web <- till_web %>% 
  left_join(ne %>% 
              filter(Review=="Tillage")) %>%
  select(-Review) %>%
  mutate(group_level1_alt = na_if(group_level1_alt, "NA")) %>%
  mutate(per_change = ifelse(grepl("%", rv_units), 
                             (trt2_value-trt1_value), 
                             (trt2_value-trt1_value)/(trt1_value)*100)) %>%
  mutate(per_change = round(per_change, digits = 2)) %>%
  grouping()

nm_web <- nm_web %>% 
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

cc_web <- sign.correction(cc_web)
till_web <- sign.correction(till_web)
nm_web <- sign.correction(nm_web)


#### WRITE FILES for Website####
write.csv(cc_web, paste0("filtered-data/ContinuousCover_Kenya_",Sys.Date(),".csv"), col.names=FALSE)
write.csv(till_web, paste0("filtered-data/Tillage_Kenya_",Sys.Date(),".csv"), col.names=FALSE)
write.csv(nm_web, paste0("filtered-data/NutrientMgmt_Kenya_",Sys.Date(),".csv"), col.names=FALSE)

#### WRITE FILES for KNB
#complete database 
write.csv(cc, paste0("KNBfiles/ContinuousCover_Kenya_",Sys.Date(),".csv"), col.names=FALSE)
write.csv(till, paste0("KNBfiles/Tillage_Kenya_",Sys.Date(),".csv"), col.names=FALSE)
write.csv(nm, paste0("KNBfiles/NutrientMgmt_Kenya_",Sys.Date(),".csv"), col.names=FALSE)
