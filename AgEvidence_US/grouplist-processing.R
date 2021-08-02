#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
library(plyr)
source('processing-functions.R')


# Data files
cc <- read_excel("data/Covercrops_AgEvidence.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_AgEvidence.xlsx", sheet = "Results")
pm <- read_excel("data/PestMgmt_AgEvidence.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_AgEvidence.xlsx", sheet = "Results")
cc_Kenya <- read_excel("C:/Users/LWA/Desktop/Cover_Kenya/ContinuousCover_AgEvidenceKenya.xlsx", sheet = "Results")


# Generate nested grouping lists
cc_groups <- cc %>% 
  select(group_level1, group_level2, group_level3) %>% unique
nm_groups <- nm %>% 
  select(group_level1, group_level2, group_level3) %>% unique
pm_groups <- pm %>% 
  select(group_level1, group_level2, group_level3) %>% unique
till_groups <- till %>% 
  select(group_level1, group_level2, group_level3) %>% unique
cc_Kenya_groups <- cc_Kenya %>% 
  select(group_level1, group_level2, group_level3) %>% unique




group_list1 <- full_join(cc_groups, nm_groups)
group_list2 <- full_join(pm_groups, till_groups)
group_lista <- full_join(group_list1, group_list2)
group_list <- full_join(group_lista, cc_Kenya_groups)

# Export csv
write.csv(group_list, paste0("filtered-data/grouplists_Kenya_",Sys.Date(),".csv"))
          