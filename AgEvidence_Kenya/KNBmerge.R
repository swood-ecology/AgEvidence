#### SET WORKING DIRECTORY ####
require(funr)
setwd(funr::get_script_path())

#### LOAD LIBRARIES ####
library(tidyverse)
library(readxl)
library(lubridate) ## will be useful for identifying most recent data file
library(openxlsx)
library(xlsx)
library(stringr)

#### READ DATA ####

# Data files
cc <- read.csv("filtered-data/Covercrops_AgEvidence_2020-08-04.csv")
nm <- read.csv("filtered-data/NutrientMgmt_AgEvidence_2020-08-04.csv")
pm <- read.csv("filtered-data/PestMgmt_AgEvidence_2020-08-04.csv")
till <- read.csv("filtered-data/Tillage_AgEvidence_2020-08-04.csv")

#### SELECT & REORDER COLS ####
filtered_cc <- cc %>% select("review",              "paper_id",
                             "duration",            "rv_year",
                             "loc_multi_results",   "group_level1",
                             "group_level2",        "group_level3",
                             "group_level1_alt",    "group_level2_alt",
                             "rv",                  "rv_depth",
                             "sample_depth",        "rv_units",
                             "stat_test",           "stat_type",
                             "trt1",                "trt1_int",
                             "trt1_int2",           "trt1_value",
                             "trt2",                "trt2_int",
                             "trt2_int2",           "trt2_value",
                             "significance",        "finelevel_group",
                             "trt1_name",           "trt1_details",
                             "trt2_name",           "trt2_details",
                             "cc_group1",           "cc_group2",
                             "per_change",          "normative_effect")

filtered_nm <- nm %>% select("review",	            "paper_id",
                             "duration",	          "rv_year",
                             "loc_multi_results",	  "group_level1",
                             "group_level2",	      "group_level3",	
                             "group_level1_alt",	  "group_level2_alt",
                             "rv",	                "rv_trtspecifics",
                             "rv_depth",	          "sample_depth",
                             "rv_units",	          "stat_test",
                             "stat_type",	          "trt1",	
                             "trt1_int",	          "trt1_int2",	
                             "trt1_value",	        "trt2",
                             "trt2_int",	          "trt2_int2",
                             "trt2_value",	        "significance",
                             "finelevel_group",	    "trt1_name",
                             "trt1_details",	      "trt2_name",
                             "trt2_details",	      "nutrient_groups",
                             "per_change",	        "normative_effect"
)

filtered_pm <- pm %>% select("review",	            "paper_id",
                             "duration",          	"rv_year",
                             "loc_multi_results",	  "group_level1",
                             "group_level2",      	"group_level3",	
                             "group_level1_alt",  	"group_level2_alt",
                             "rv",                 	"rv_depth",
                             "sample_depth",      	"rv_units",
                             "stat_test",         	"stat_type",
                             "trt1",              	"trt1_int",
                             "trt1_int2",	          "trt1_value",
                             "trt2",              	"trt2_int",
                             "trt2_int2",         	"trt2_value",
                             "significance",      	"finelevel_group",
                             "trt1_name",         	"trt1_details",
                             "trt2_name",         	"trt2_details",
                             "pm_group1",         	"pm_group2",
                             "per_change",	        "normative_effect"
)

filtered_till <- till %>% select("review",	            "paper_id",
                                 "duration",          	"rv_year",
                                 "loc_multi_results",	  "group_level1",
                                 "group_level2",      	"group_level3",	
                                 "group_level1_alt",  	"group_level2_alt",
                                 "rv",                 	"rv_depth",
                                 "sample_depth",      	"rv_units",
                                 "stat_test",         	"stat_type",
                                 "trt1",              	"trt1_int",
                                 "trt1_int2",	          "trt1_value",
                                 "trt2",              	"trt2_int",
                                 "trt2_int2",         	"trt2_value",
                                 "significance",      	"finelevel_group",
                                 "trt1_name",         	"trt1_details",
                                 "trt2_name",         	"trt2_details",
                                 "tillage_1",          	"tillage_2",
                                 "trt_compare",	        "per_change",
                                 "normative_effect") 




#### STANDARDIZE NORMATIVE EFFECTS ####

#Cover Crops
filtered_cc$normative_effect <-  str_replace_all(filtered_cc$normative_effect, "assumed", "Assumed")
filtered_cc$normative_effect <-  str_replace_all(filtered_cc$normative_effect, "dependent", "Dependent")
filtered_cc$normative_effect <-  str_replace_all(filtered_cc$normative_effect, "Positive", "Assumed societal benefit")
filtered_cc$normative_effect <-  str_replace_all(filtered_cc$normative_effect, "Negative", "Assumed societal harm")

#Nutrient Management
filtered_nm$normative_effect <-  str_replace_all(filtered_nm$normative_effect, "assumed", "Assumed")
filtered_nm$normative_effect <-  str_replace_all(filtered_nm$normative_effect, "dependent", "Dependent")
filtered_nm$normative_effect <-  str_replace_all(filtered_nm$normative_effect, "Positive", "Assumed societal benefit")
filtered_nm$normative_effect <-  str_replace_all(filtered_nm$normative_effect, "Negative", "Assumed societal harm")

#Pest Management
filtered_pm$normative_effect <-  str_replace_all(filtered_pm$normative_effect, "assumed", "Assumed")
filtered_pm$normative_effect <-  str_replace_all(filtered_pm$normative_effect, "dependent", "Dependent")
filtered_pm$normative_effect <-  str_replace_all(filtered_pm$normative_effect, "Positive", "Assumed societal benefit")
filtered_pm$normative_effect <-  str_replace_all(filtered_pm$normative_effect, "Negative", "Assumed societal harm")

#Tillage
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "assumed", "Assumed")
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "dependent", "Dependent")
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "Positive", "Assumed societal benefit")
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "Negative", "Assumed societal harm")


#### CREATE UPDATED WORKBOOK FOR EACH REVIEW ####

#Cover Crops
cc_wb <- loadWorkbook("data/Covercrops_AgEvidence.xlsx")
openxlsx::removeWorksheet(cc_wb, "Results")
openxlsx::addWorksheet(cc_wb, "Results")
openxlsx::writeData(cc_wb, "Results", filtered_cc)
openxlsx::saveWorkbook(cc_wb, "KNBfiles/Covercrops_AgEvidence.xlsx", overwrite = T)

#Nutrient Management
pm_wb <- loadWorkbook("data/NutrientMgmt_AgEvidence.xlsx")
openxlsx::removeWorksheet(pm_wb, "Results")
openxlsx::addWorksheet(pm_wb, "Results")
openxlsx::writeData(pm_wb, "Results", filtered_pm)
openxlsx::saveWorkbook(pm_wb, "KNBfiles/NutrientMgmt_AgEvidence.xlsx", overwrite = T)

#Pest Management
pm_wb <- loadWorkbook("data/PestMgmt_AgEvidence.xlsx")
openxlsx::removeWorksheet(pm_wb, "Results")
openxlsx::addWorksheet(pm_wb, "Results")
openxlsx::writeData(pm_wb, "Results", filtered_pm)
openxlsx::saveWorkbook(pm_wb, "KNBfiles/PestMgmt_AgEvidence.xlsx", overwrite = T)


#Tillage
till_wb <- loadWorkbook("data/Tillage_AgEvidence.xlsx")
openxlsx::removeWorksheet(till_wb, "Results")
openxlsx::addWorksheet(till_wb, "Results")
openxlsx::writeData(till_wb, "Results", filtered_till)
openxlsx::saveWorkbook(till_wb, "KNBfiles/Tillage_AgEvidence.xlsx", overwrite = T)
