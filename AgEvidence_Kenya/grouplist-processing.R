#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
library(plyr)


# Data files
cc <- read_excel("data/ContinuousCover_Kenya.xlsx", sheet = "Results")
nm <- read_excel("data/NutrientMgmt_AgEvidenceKenya_Kate.xlsx", sheet = "Results")
till <- read_excel("data/Tillage_Kenya.xlsx", sheet="Results")


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

          