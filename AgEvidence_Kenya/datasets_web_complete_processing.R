# Split datasets into website and complete versions
#always add data to complete version and then convert complete version to website version using the following code.

library(readxl)
library(dplyr)

cc_k <- read_excel("data/ContinuousCover_Kenya_complete.xlsx", sheet = "Results")
nm_k <- read_excel("data/NutrientMgmt_Kenya_complete.xlsx", sheet = "Results")


# CC remove alley crop, intercrop, monocrop-monocrop from website version

cc_k_new <- cc_k %>%
            filter(trt1_name != "alley crop", 
                   trt1_name != "intercrop") %>% #drops 200 rows
            mutate(trt_namescombo = paste(trt1_name, " ",trt2_name)) %>%
            filter(trt_namescombo != "monocrop   monocrop") %>%
            select(-trt_namescombo)


# NM remove trt1_name == "NA"
nm_k_web <- nm_k %>%
            filter(trt1_name != "NA")



#### CREATE UPDATED Website WORKBOOK FOR EACH REVIEW ####
library(openxlsx)

#Cover Crops
cc_wb <- loadWorkbook("data/ContinuousCover_Kenya_complete.xlsx")
openxlsx::removeWorksheet(cc_wb, "Results")
openxlsx::addWorksheet(cc_wb, "Results")
openxlsx::writeData(cc_wb, "Results", cc_k_new)
openxlsx::saveWorkbook(cc_wb, "data/ContinuousCover_Kenya_website.xlsx", overwrite = T)
#manually remove paper_id 18 & 29

#Nutrient Management
nm_wb <- loadWorkbook("data/NutrientMgmt_Kenya_complete.xlsx")
openxlsx::removeWorksheet(nm_wb, "Results")
openxlsx::addWorksheet(nm_wb, "Results")
openxlsx::writeData(nm_wb, "Results", nm_k_web)
openxlsx::saveWorkbook(nm_wb, "data/NutrientMgmt_Kenya_website.xlsx", overwrite = T)
#manually check for missing paper id's that don't move over.
#remove paper_id = 5009 & 5013


#no adjustments made to Tillage. website = complete dataset
#Tillage
till_wb <- loadWorkbook("data/Tillage_Kenya_081721.xlsx")
openxlsx::removeWorksheet(till_wb, "Results")
openxlsx::addWorksheet(till_wb, "Results")
openxlsx::writeData(till_wb, "Results", filtered_till)
openxlsx::saveWorkbook(till_wb, "KNBfiles/Tillage_AgEKenya.xlsx", overwrite = T)

#double check all paper id's are included in results page for each practice

