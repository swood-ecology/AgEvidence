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

##need to update data file names####

# Data files
cc <- read.csv("filtered-data/ContinuousCover_Kenya_2021-12-02.csv")
nm <- read.csv("filtered-data/NutrientMgmt_Kenya_2021-12-02.csv")
till <- read.csv("filtered-data/Tillage_Kenya_2021-12-02.csv")

colnames(till)

#### SELECT & REORDER COLS ####
filtered_cc <- cc %>% select("review",            "paper_id",          "duration",
                             "rv_year",           "loc_multi_results", "group_level1",
                             "group_level2",      "group_level3",      "rv",
                             "rv_depth",          "sample_depth",      "rv_units",
                             "stat_test",         "stat_type",         "trt1",
                             "trt1_int",          "trt1_int2",         "trt1_value",
                             "trt2",              "trt2_int",          "trt2_int2",
                             "trt2_value",        "significance",      "trt1_name",
                             "trt1_details",      "trt2_name",         "trt2_details",
                             "trt2_group1",       "trt2_group2",       "crop_type",
                             "system_group1",     "system_group2",     "mgmt_intention",
                             "norm_interp2",      "norm_interp3",      "group_level1_alt",
                             "group_level2_alt",  "norm_interp2_alt",  "norm_interp3_alt",
                             "per_change",        "grouping",          "normative_effect" )

filtered_nm <- nm %>% select("review",            "paper_id",          "duration",
                             "rv_year",           "loc_multi_results", "group_level1",
                             "group_level2",      "group_level3",      "rv",
                             "rv_timeseries",     "rv_trtspecifics",
                             "rv_depth",          "sample_depth",      "rv_units",
                             "stat_test",         "stat_type",         "trt1",
                             "trt1_int",          "trt1_int2",         "trt1_value",
                             "trt2",              "trt2_int",          "trt2_int2",
                             "trt2_value",        "significance",      "trt1_name",
                             "trt1_details",      "trt1_NPKC_groups",
                             "trt2_name",         "trt2_details",      "trt2_NPKC_groups",
                             "crop_type",         "amendment_groups",
                             "norm_interp2",      "norm_interp3",      "group_level1_alt",
                             "group_level2_alt",  "norm_interp2_alt",  "norm_interp3_alt",
                             "per_change",        "grouping",          "normative_effect")



filtered_till <- till %>% select("review",            "paper_id",          "duration",
                                 "rv_year",           "loc_multi_results", "group_level1",
                                 "group_level2",      "group_level3",      "rv",
                                 "rv_timeseries",   
                                 "rv_depth",          "sample_depth",      "rv_units",
                                 "stat_test",         "stat_type",         "trt1",
                                 "trt1_int",          "trt1_int2",         "trt1_value",
                                 "trt2",              "trt2_int",          "trt2_int2",
                                 "trt2_value",        "significance",      "finelevel_group",
                                 "trt1_name",
                                 "trt1_details",      "trt2_name",         "trt2_details",
                                 "tillage_1",         "tillage_2",         "trt_compare",
                                 "crop_type",
                                 "norm_interp2",      "norm_interp3",      "group_level1_alt",
                                 "group_level2_alt",  "norm_interp2_alt",  "norm_interp3_alt",
                                 "per_change",        "grouping",          "normative_effect") 




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

#Tillage
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "assumed", "Assumed")
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "dependent", "Dependent")
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "Positive", "Assumed societal benefit")
filtered_till$normative_effect <-  str_replace_all(filtered_till$normative_effect, "Negative", "Assumed societal harm")


#### CREATE UPDATED WORKBOOK FOR EACH REVIEW ####

#Cover Crops
cc_wb <- loadWorkbook("data/ContinuousCover_Kenya_081721.xlsx")
openxlsx::removeWorksheet(cc_wb, "Results")
openxlsx::addWorksheet(cc_wb, "Results")
openxlsx::writeData(cc_wb, "Results", filtered_cc)
openxlsx::saveWorkbook(cc_wb, "KNBfiles/ContinuousCover_AgEKenya.xlsx", overwrite = T)

#Nutrient Management
nm_wb <- loadWorkbook("data/NutrientMgmt_Kenya_CURRENT_081721.xlsx")
openxlsx::removeWorksheet(nm_wb, "Results")
openxlsx::addWorksheet(nm_wb, "Results")
openxlsx::writeData(nm_wb, "Results", filtered_nm)
openxlsx::saveWorkbook(nm_wb, "KNBfiles/NutrientMgmt_AgEKenya.xlsx", overwrite = T)

#Tillage
till_wb <- loadWorkbook("data/Tillage_Kenya_081721.xlsx")
openxlsx::removeWorksheet(till_wb, "Results")
openxlsx::addWorksheet(till_wb, "Results")
openxlsx::writeData(till_wb, "Results", filtered_till)
openxlsx::saveWorkbook(till_wb, "KNBfiles/Tillage_AgEKenya.xlsx", overwrite = T)
