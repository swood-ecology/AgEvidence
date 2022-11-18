#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#test

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
library(plyr)
library(ggplot2)

# Data files, Excel Format ######
#cc <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
#nm <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")
#till <- read_excel("data/Tillage_Kenya_complete.xlsx", sheet="Results")


#Import Csv format ####

cc <- read.csv("data/ContinuousCover/ContinuousCover_Kenya_complete_Results.csv")
nm <- read.csv("data/NutrientMgmt/NutrientMgmt_Kenya_complete_Results.csv")
till <- read.csv("data/Tillage/Tillage_Kenya_complete_Results.csv")



# Generate nested grouping lists
cc_groups <- cc %>% 
  select(group_level1, group_level2, group_level3) %>% unique
nm_groups <- nm %>% 
  select(group_level1, group_level2, group_level3) %>% unique
till_groups <- till %>% 
  select(group_level1, group_level2, group_level3) %>% unique # grouping, normative_effect

group_list1 <- full_join(cc_groups, nm_groups)
group_lista <- full_join(group_list1, till_groups)



# Generate list of group discrepancies
unique(unlist(strsplit(as.character(cc$mgmt_intention), ";")))
unique(unlist(strsplit(as.character(nm$group_level2), ";")))
unique(unlist(strsplit(as.character(till$group_level3), ";")))

#Generate list of unique crops
# Data files - Cash crop tab
cc_crops <- read.csv("data/ContinuousCover/ContinuousCover_Kenya_complete_Cashcrop.csv")
nm_crops <- read.csv("data/NutrientMgmt/NutrientMgmt_Kenya_complete_Cashcrop.csv")
till_crops <- read.csv("data/Tillage/Tillage_Kenya_complete_Cashcrop.csv")

cc_cropslist <- unique(unlist(strsplit(as.character(cc_crops$control_species), ";")))
nm_cropslist <- unique(unlist(strsplit(as.character(nm_crops$control_species), ";")))
till_cropslist <- unique(unlist(strsplit(as.character(till_crops$cash_species), ";")))

crops <- unique(c(as.character(cc_cropslist), as.character(nm_cropslist), as.character(till_cropslist)))
cropslist <- as.data.frame(crops)





###########################################

#explore website viewing data 
#which practices get the most views?

# Data files
sitedata <- read_excel("/Users/lesley.atwood/Desktop/Active projects/AgEvidence/AgEvidenceSiteData.xlsx", 
                       sheet = "Dataset1")

#new cols
sitedata$GeoPrac <- paste(sitedata$Geography, sep=", ", sitedata$Practice)

#Split by Geography
US <- sitedata[sitedata$Geography %in% "US",]
Kenya <- sitedata[sitedata$Geography %in% "Kenya",]


USplot <- boxplot(UniquePageviews~Practice, data = US )
 #Tillage, Covercrops, NM, Pests

Kenyaplot <- boxplot(UniquePageviews~Practice, data = Kenya )
 #NutrientMgmt, Tillage, ContinousCover






          