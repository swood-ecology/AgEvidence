#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)

#### FUNCTIONS ####
# Function to change the GL2 changes in the raw data
gl2.rename <- function(data) {
  data %>%
    mutate(group_level2 =
             ifelse(group_level1=="Other Soil Properties" & 
                      (group_level3=="Aggregate size"|group_level3=="Aggregate stability"|
                         group_level3=="Air-filled pore space"|group_level3=="Air-filled pores"|
                         group_level3=="Total pore space"|group_level3=="Water-filled pore space"), 
                    "Soil Structure",
                    group_level2)
    ) %>%
    mutate(group_level2 = 
             ifelse(group_level1=="Other Soil Properties" & 
                      group_level3=="Decomposition rate of surface residue",
                    "Biotic Factors",
                    group_level2)) %>%
    mutate(group_level2 = 
             ifelse(group_level1=="Other Soil Properties" & group_level3=="Soil organic matter content",
                    "Chemical Properties",
                    group_level2))
}
# Function to generate two separate columns
# based on GLs and NEs
grouping <- function(data) {
  data %>%
    mutate(grouping=
             ifelse(is.na(group_level1_alt), 
                    paste(group_level1,"|",group_level2,"|",group_level3),
                    paste(group_level1,"|",group_level2,"|",group_level3,";",
                          group_level1_alt,"|",group_level2_alt,"|",group_level3)
             ),
           normative_effect=
             ifelse(is.na(group_level1_alt),
                    paste(norm_interp2,"|",norm_interp3),
                    paste(norm_interp2,"|",norm_interp3,";",
                          norm_interp2_alt,"|",norm_interp3_alt)
             )
    )
}


#### READ DATA ####
# Normative effects files
ne <- read_excel("Normative_effects_groups.xlsx")
ne_mod <- read_excel("Normative_effects_groups_modified.xlsx")
ne_mod_2 <- read_excel("Normative_effects_groups_modified_2.xlsx")
ne_mod_3 <- read_excel("10Mar20_Normative_effects_groups_modified_2.xlsx")

# Data files
cc <- read_excel("Covercrops_AgEvidence.xlsx", sheet = "Results")
nm <- read_excel("NutrientMgmt_AgEvidence.xlsx", sheet = "Results")
pm <- read_excel("PestMgmt_AgEvidence.xlsx", sheet = "Results")
till <- read_excel("Tillage_AgEvidence.xlsx", sheet = "Results")

# #### DATA CHECKING ####
# # Check which grouping vars differ among files
# setdiff(ne_mod_2 %>% select(group_level2) %>% unique() %>% arrange(group_level2),
#         ne_mod %>% select(group_level2) %>% unique() %>% arrange(group_level2)  
# )
# 
# setdiff(ne_mod_2 %>% select(group_level3) %>% unique() %>% arrange(group_level3),
#         ne_mod %>% select(group_level3) %>% unique() %>% arrange(group_level3)  
# )
rm(ne); rm(ne_mod); rm(ne_mod_2)

#### GL2 RENAMING ####
cc <- gl2.rename(cc) 
till <- gl2.rename(till)
nm <- gl2.rename(nm)
pm <- gl2.rename(pm)

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
              filter(Review=="Cover crop")) %>%
  select(-NOTES,-Review) %>%
  grouping()

till <- till %>% 
  full_join(ne_mod_3 %>%
              filter(Review=="Tillage")) %>%
  select(-NOTES,-Review) %>%
  grouping()

nm <- nm %>% 
  full_join(ne_mod_3 %>%
              filter(Review=="Nutrient Management")) %>%
  select(-NOTES,-Review) %>%
  grouping()

pm <- pm %>% 
  full_join(ne_mod_3 %>%
              filter(Review=="Early Season Pest Management")) %>%
  select(-NOTES,-Review) %>%
  grouping()

